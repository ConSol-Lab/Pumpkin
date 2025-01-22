use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;

use clap::Parser;
use convert_case::Case;
use itertools::Itertools;
use log::info;
use log::LevelFilter;
use petgraph::adj::List;
use petgraph::algo::toposort;
use petgraph::algo::tred::dag_to_toposorted_adjacency_list;
use petgraph::algo::tred::dag_transitive_reduction_closure;
use petgraph::Directed;
use petgraph::Graph;
use pumpkin_solver::branching::branchers::alternating_brancher::AlternatingBrancher;
use pumpkin_solver::branching::branchers::alternating_brancher::AlternatingStrategy::SwitchToDefaultAfterFirstSolution;
use pumpkin_solver::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_solver::branching::value_selection::InDomainMin;
use pumpkin_solver::branching::variable_selection::Smallest;
use pumpkin_solver::constraints;
use pumpkin_solver::containers::KeyedVec;
use pumpkin_solver::containers::StorageKey;
use pumpkin_solver::optimisation::LowerBoundingSearch;
use pumpkin_solver::optimisation::OptimisationDirection;
use pumpkin_solver::optimisation::UpperBoundingSearch;
use pumpkin_solver::options::ConflictResolver;
use pumpkin_solver::options::CumulativeOptions;
use pumpkin_solver::options::CumulativePropagationMethod;
use pumpkin_solver::options::LearnedNogoodSortingStrategy;
use pumpkin_solver::options::LearningOptions;
use pumpkin_solver::options::RestartOptions;
use pumpkin_solver::options::SolverOptions;
use pumpkin_solver::proof::ProofLog;
use pumpkin_solver::results::ProblemSolution;
use pumpkin_solver::statistics::configure_statistic_logging;
use pumpkin_solver::termination::Combinator;
use pumpkin_solver::termination::DecisionBudget;
use pumpkin_solver::termination::OsSignal;
use pumpkin_solver::termination::TimeBudget;
use pumpkin_solver::variables::DomainId;
use pumpkin_solver::variables::Literal;
use pumpkin_solver::variables::TransformableVariable;
use pumpkin_solver::Solver;
use rand::rngs::SmallRng;
use rand::SeedableRng;
use rcpsp_instance::parse_rcpsp_dzn;
use rcpsp_instance::SchedulingError;
use rcpsp_instance::SchedulingResult;

mod minizinc_data_parser;
mod rcpsp_instance;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The instance to use for bound evaluation. Must be formatted as a MiniZinc data (*.dzn)
    /// file.
    instance_path: PathBuf,

    /// Enable the node-packing propagator
    #[arg(short = 'n', long)]
    use_node_packing: bool,

    /// Enables the cumulative to mine for disjointness
    #[arg(short = 'c', long)]
    use_cumulative_disjointness: bool,

    #[arg(short = 'o', long)]
    use_nogood_disjointness: bool,

    /// The maximum number of rotations performed by the node-packing propagator
    #[arg(short='o', long, default_value_t=usize::MAX)]
    number_of_cycles: usize,

    #[arg(short = 't', long = "time-limit")]
    time_limit: Option<u64>,

    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    /// Determines whether to use fixed search (smallest, indomain-min) or a strategy alternating
    /// between (smallest, indomain-min), and VSIDS
    #[arg(short = 'f', long)]
    use_fixed_search: bool,

    #[arg(short = 'd', long = "decision-limit")]
    decision_limit: Option<u64>,

    /// Determines whether to allow clause database removal
    #[arg(short = 'r', long = "no-removal")]
    no_removal: bool,

    #[arg(short = 'l', long = "use-lower-bounding-search")]
    use_lower_bounding_search: bool,
}

pub fn main() {
    match run() {
        Ok(_) => {}
        Err(e) => {
            println!("Execution failed, error: {e:#?}");
            std::process::exit(1);
        }
    }
}

fn create_transitive_closure_of_graph(
    edges: &[Vec<usize>],
    number_of_tasks: u32,
) -> (List<(), usize>, Vec<usize>) {
    let mut graph: Graph<usize, usize, Directed, usize> = Graph::from_edges(
        edges
            .iter()
            .enumerate()
            .filter_map(|(index, dependencies)| {
                if dependencies.is_empty() {
                    return None;
                }
                Some(
                    dependencies
                        .iter()
                        .map(|dependency| (index, *dependency))
                        .collect::<Vec<_>>(),
                )
            })
            .flatten(),
    );

    while graph.node_count() != number_of_tasks as usize {
        let _ = graph.add_node(1);
    }

    let (toposorted_map, rev_map): (List<(), usize>, Vec<usize>) =
        dag_to_toposorted_adjacency_list(&graph, &toposort(&graph, None).unwrap());

    let (_, transitive_closure) = dag_transitive_reduction_closure(&toposorted_map);
    (transitive_closure, rev_map)
}

fn run() -> SchedulingResult<()> {
    let args = Args::parse();

    if !args.use_node_packing && (args.use_cumulative_disjointness || args.use_nogood_disjointness)
    {
        panic!("Node packing is disabled but cumulative or nogood disjointness mining is activated")
    }

    if args.instance_path.extension().and_then(|ext| ext.to_str()) != Some("dzn") {
        return Err(SchedulingError::invalid_instance(
            args.instance_path.display(),
        ));
    }
    let instance_path = args
        .instance_path
        .to_str()
        .ok_or(SchedulingError::invalid_instance(
            args.instance_path.display(),
        ))?;

    let instance_file = File::open(instance_path)?;
    let rcpsp_instance = parse_rcpsp_dzn(instance_file)?;

    configure_statistic_logging(
        "%%%mzn-stat:",
        Some("%%%mzn-stat-end"),
        Some(Case::Camel),
        None,
    );
    let level_filter = if args.verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };

    env_logger::Builder::new()
        .format(move |buf, record| {
            write!(buf, "% ")?;

            writeln!(buf, "{}", record.args())
        })
        .filter_level(level_filter)
        .target(env_logger::Target::Stdout)
        .init();
    info!("Logging successfully configured");

    let mut solver = Solver::with_options(SolverOptions {
        restart_options: RestartOptions::default(),
        learning_clause_minimisation: true,
        random_generator: SmallRng::seed_from_u64(42),
        proof_log: ProofLog::default(),
        conflict_resolver: ConflictResolver::default(),
        learning_options: if args.no_removal {
            LearningOptions {
                max_activity: 1e20,
                activity_decay_factor: 0.99,
                limit_num_high_lbd_nogoods: 4000,
                nogood_sorting_strategy: LearnedNogoodSortingStrategy::Lbd,
                lbd_threshold: u32::MAX,
                activity_bump_increment: 1.0,
            }
        } else {
            LearningOptions::default()
        },
    });

    let ub_variables: i32 = rcpsp_instance
        .processing_times
        .iter()
        .map(|&processing_time| processing_time as i32)
        .sum();

    let makespan = solver.new_bounded_integer(0, ub_variables);
    let start_variables = (0..rcpsp_instance.processing_times.len())
        .map(|task_index| {
            solver.new_bounded_integer(
                0,
                ub_variables - rcpsp_instance.processing_times[task_index] as i32,
            )
        })
        .collect::<Vec<_>>();

    let result = solver
        .add_constraint(constraints::maximum(
            start_variables
                .iter()
                .enumerate()
                .map(|(index, start_variable)| {
                    start_variable.offset(rcpsp_instance.processing_times[index] as i32)
                }),
            makespan,
        ))
        .post();
    if result.is_err() {
        panic!("Adding precedence for makespan led to unsatisfiability");
    }

    let (transitive_closure, rev_map) = create_transitive_closure_of_graph(
        &rcpsp_instance.dependencies,
        rcpsp_instance.processing_times.len() as u32,
    );

    let mut incompatibility_matrix: Vec<Vec<Literal>> =
        Vec::with_capacity(rcpsp_instance.processing_times.len());

    let mut mapping: KeyedVec<DomainId, usize> = KeyedVec::default();

    for index in 0..rcpsp_instance.processing_times.len() {
        while mapping.len() <= start_variables[index].index() {
            let _ = mapping.push(usize::MAX);
        }
        mapping[start_variables[index]] = index;

        let mut new_vec = Vec::with_capacity(rcpsp_instance.processing_times.len());
        for other_index in 0..rcpsp_instance.processing_times.len() {
            let result = match index.cmp(&other_index) {
                std::cmp::Ordering::Less => {
                    let mut is_resource_infeasible = false;
                    for resource_index in 0..rcpsp_instance.resource_capacities.len() {
                        if rcpsp_instance.resource_requirements[resource_index][index]
                            + rcpsp_instance.resource_requirements[resource_index][other_index]
                            > rcpsp_instance.resource_capacities[resource_index]
                        {
                            is_resource_infeasible = true;
                            break;
                        }
                    }
                    let mut is_connected_by_precedence = false;
                    if !is_resource_infeasible
                        && (transitive_closure.contains_edge(rev_map[index], rev_map[other_index])
                            || transitive_closure
                                .contains_edge(rev_map[other_index], rev_map[index]))
                    {
                        is_connected_by_precedence = true;
                    }

                    if is_resource_infeasible || is_connected_by_precedence {
                        solver.get_true_literal()
                    } else {
                        solver.new_literal()
                    }
                }
                std::cmp::Ordering::Equal => solver.get_false_literal(),
                std::cmp::Ordering::Greater => incompatibility_matrix[other_index][index],
            };
            new_vec.push(result);
        }
        incompatibility_matrix.push(new_vec)
    }

    if args.use_node_packing && args.use_nogood_disjointness {
        solver.add_incompatibility(Some(incompatibility_matrix.clone()), Some(mapping.clone()));
    }

    for (task_index, dependencies) in rcpsp_instance.dependencies.iter().enumerate() {
        for dependency in dependencies.iter() {
            let result = solver
                .add_constraint(constraints::binary_less_than_or_equals(
                    start_variables[*dependency]
                        .offset(rcpsp_instance.processing_times[*dependency] as i32),
                    start_variables[task_index].scaled(1),
                ))
                .post();
            if result.is_err() {
                panic!("Adding precedence led to unsatisfiability");
            }
        }
    }

    for (resource_index, resource_usages) in rcpsp_instance.resource_requirements.iter().enumerate()
    {
        let result = solver
            .add_constraint(constraints::cumulative_with_options(
                start_variables.clone(),
                rcpsp_instance
                    .processing_times
                    .iter()
                    .map(|&value| value as i32)
                    .collect::<Vec<_>>(),
                resource_usages
                    .iter()
                    .map(|&value| value as i32)
                    .collect::<Vec<_>>(),
                rcpsp_instance.resource_capacities[resource_index] as i32,
                CumulativeOptions::new(
                    false,
                    pumpkin_solver::options::CumulativeExplanationType::BigStep,
                    false,
                    CumulativePropagationMethod::default(),
                    false,
                    if args.use_cumulative_disjointness {
                        Some(incompatibility_matrix.clone())
                    } else {
                        None
                    },
                ),
            ))
            .post();
        if result.is_err() {
            panic!("Adding cumulative led to unsatisfiability");
        }
    }

    if args.use_node_packing {
        let result = solver
            .add_constraint(constraints::node_packing(
                &start_variables,
                &rcpsp_instance
                    .processing_times
                    .iter()
                    .map(|&value| value as i32)
                    .collect::<Vec<_>>(),
                args.number_of_cycles,
                makespan,
                incompatibility_matrix.clone(),
            ))
            .post();
        if result.is_err() {
            panic!("Adding node packing bound led to unsatisfiability");
        }
    }

    let other = start_variables.clone();

    solver.with_solution_callback(move |callback_arguments| {
        println!("-----------------------------------------");
        callback_arguments.log_statistics();
        println!(
            "Found solution with makespan {} - {:?}",
            callback_arguments.solution.get_integer_value(makespan),
            other
                .iter()
                .map(|variable| callback_arguments.solution.get_integer_value(*variable))
                .collect::<Vec<_>>()
        );
    });

    let mut termination = Combinator::new(
        args.decision_limit
            .map(|decision_limit| DecisionBudget::new(decision_limit)),
        Combinator::new(
            OsSignal::install(),
            args.time_limit
                .map(|time| TimeBudget::starting_now(Duration::from_secs(time))),
        ),
    );

    let result = if args.use_fixed_search {
        info!("Using fixed search");
        let mut brancher = IndependentVariableValueBrancher::new(
            Smallest::new(
                &start_variables
                    .into_iter()
                    .chain(std::iter::once(makespan))
                    .collect::<Vec<_>>(),
            ),
            InDomainMin,
        );
        if args.use_lower_bounding_search {
            solver.optimise(
                &mut brancher,
                &mut termination,
                makespan,
                OptimisationDirection::Minimise,
                LowerBoundingSearch,
            )
        } else {
            solver.optimise(
                &mut brancher,
                &mut termination,
                makespan,
                OptimisationDirection::Minimise,
                UpperBoundingSearch,
            )
        }
    } else {
        info!("Using alternating search");
        let mut brancher = AlternatingBrancher::with_blacklist(
            &solver,
            IndependentVariableValueBrancher::new(
                Smallest::new(
                    &start_variables
                        .into_iter()
                        .chain(std::iter::once(makespan))
                        .collect::<Vec<_>>(),
                ),
                InDomainMin,
            ),
            &incompatibility_matrix
                .iter()
                .flat_map(|row| row.iter().map(|lit| lit.domain_id()))
                .collect_vec(),
            SwitchToDefaultAfterFirstSolution,
        );
        if args.use_lower_bounding_search {
            solver.optimise(
                &mut brancher,
                &mut termination,
                makespan,
                OptimisationDirection::Minimise,
                LowerBoundingSearch,
            )
        } else {
            solver.optimise(
                &mut brancher,
                &mut termination,
                makespan,
                OptimisationDirection::Minimise,
                UpperBoundingSearch,
            )
        }
    };

    println!("------------------Final Statistics------------------");
    solver.log_statistics();

    match result {
        pumpkin_solver::results::OptimisationResult::Optimal(solution) => {
            println!(
                "Found optimal solution with makespan {}",
                solution.get_integer_value(makespan)
            )
        }
        pumpkin_solver::results::OptimisationResult::Satisfiable(solution) => {
            println!(
                "Found satisfiable solution with makespan {}",
                solution.get_integer_value(makespan)
            )
        }
        pumpkin_solver::results::OptimisationResult::Unsatisfiable => {
            println!("Unsatisfiable")
        }
        pumpkin_solver::results::OptimisationResult::Unknown => {
            println!("Unknown")
        }
    }
    Ok(())
}
