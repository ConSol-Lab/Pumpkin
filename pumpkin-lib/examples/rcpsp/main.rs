use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;

use clap::Parser;
use convert_case::Case;
use log::info;
use log::LevelFilter;
use petgraph::adj::List;
use petgraph::algo::toposort;
use petgraph::algo::tred::dag_to_toposorted_adjacency_list;
use petgraph::algo::tred::dag_transitive_reduction_closure;
use petgraph::Directed;
use petgraph::Graph;
use pumpkin_lib::branching::branchers::alternating_brancher::AlternatingBrancher;
use pumpkin_lib::branching::branchers::independent_variable_value_brancher::IndependentVariableValueBrancher;
use pumpkin_lib::branching::InDomainMin;
use pumpkin_lib::branching::Smallest;
use pumpkin_lib::constraints;
use pumpkin_lib::results::ProblemSolution;
use pumpkin_lib::termination::Combinator;
use pumpkin_lib::termination::OsSignal;
use pumpkin_lib::termination::TimeBudget;
use pumpkin_lib::variables::TransformableVariable;
use pumpkin_lib::Solver;
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

    /// Enable the parallel-machine propagator
    #[arg(short = 'p', long)]
    use_parallel_machine: bool,

    /// Enable the node-packing propagator
    #[arg(short = 'n', long)]
    use_node_packing: bool,

    /// The maximum number of rotations performed by the node-packing propagator
    #[arg(short='c', long, default_value_t=usize::MAX)]
    number_of_cycles: usize,

    /// The minimum number of machines passed to the parallel-machine propagator
    #[arg(short = 'i', long, default_value_t = 2)]
    minimum_number_of_machines: usize,

    /// The maximum number of machines passed to the parallel-machine propagator
    #[arg(short = 'a', long, default_value_t = 5)]
    maximum_number_of_machines: usize,

    #[arg(short = 't', long = "time-limit")]
    time_limit: Option<u64>,

    #[arg(short = 'v', long = "verbose")]
    verbose: bool,
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

    pumpkin_lib::statistics::configure(
        true,
        "%%%mzn-stat:",
        Some("%%%mzn-stat-end"),
        Some(Case::Camel),
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

    let mut solver = Solver::default();

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

    let mut incompatibility_matrix = vec![
        vec![false; rcpsp_instance.processing_times.len()];
        rcpsp_instance.processing_times.len()
    ];

    let (transitive_closure, rev_map) = create_transitive_closure_of_graph(
        &rcpsp_instance.dependencies,
        rcpsp_instance.processing_times.len() as u32,
    );

    // Keep track of the resource infeasibilities and the incompatibilities due to precedence
    // constraints
    for index in 0..rcpsp_instance.processing_times.len() {
        for other_index in 0..rcpsp_instance.processing_times.len() {
            if index == other_index {
                continue;
            }
            for resource_index in 0..rcpsp_instance.resource_capacities.len() {
                if rcpsp_instance.resource_requirements[resource_index][index]
                    + rcpsp_instance.resource_requirements[resource_index][other_index]
                    > rcpsp_instance.resource_capacities[resource_index]
                {
                    incompatibility_matrix[index][other_index] = true;
                    incompatibility_matrix[other_index][index] = true;
                }
            }
            if transitive_closure.contains_edge(rev_map[index], rev_map[other_index])
                || transitive_closure.contains_edge(rev_map[other_index], rev_map[index])
            {
                incompatibility_matrix[index][other_index] = true;
                incompatibility_matrix[other_index][index] = true;
            }
        }
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
            .add_constraint(constraints::cumulative(
                &start_variables,
                &rcpsp_instance
                    .processing_times
                    .iter()
                    .map(|&value| value as i32)
                    .collect::<Vec<_>>(),
                &resource_usages
                    .iter()
                    .map(|&value| value as i32)
                    .collect::<Vec<_>>(),
                rcpsp_instance.resource_capacities[resource_index] as i32,
            ))
            .post();
        if result.is_err() {
            panic!("Adding cumulative led to unsatisfiability");
        }

        if args.use_parallel_machine {
            let result = solver
                .add_constraint(constraints::parallel_machine(
                    &start_variables,
                    &rcpsp_instance
                        .processing_times
                        .iter()
                        .map(|&value| value as i32)
                        .collect::<Vec<_>>(),
                    &resource_usages
                        .iter()
                        .map(|&value| value as i32)
                        .collect::<Vec<_>>(),
                    rcpsp_instance.resource_capacities[resource_index] as i32,
                    args.minimum_number_of_machines,
                    args.maximum_number_of_machines,
                    makespan,
                ))
                .post();
            if result.is_err() {
                panic!("Adding parallel machine bound led to unsatisfiability");
            }
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

    solver.with_solution_callback(move |solution| {
        println!("-----------------------------------------");
        println!(
            "Found solution with makespan {}",
            solution.get_integer_value(makespan)
        );
    });

    let mut brancher = AlternatingBrancher::new(&solver, IndependentVariableValueBrancher::new(Smallest::new(&start_variables.into_iter().chain(std::iter::once(makespan)).collect::<Vec<_>>()), InDomainMin), pumpkin_lib::branching::branchers::alternating_brancher::AlternatingStrategy::SwitchToDefaultAfterFirstSolution);

    let mut termination = Combinator::new(
        OsSignal::install(),
        args.time_limit
            .map(|time| TimeBudget::starting_now(Duration::from_secs(time))),
    );

    match solver.minimise(&mut brancher, &mut termination, makespan) {
        pumpkin_lib::results::OptimisationResult::Optimal(solution) => {
            println!(
                "Found optimal solution with makespan {}",
                solution.get_integer_value(makespan)
            )
        }
        pumpkin_lib::results::OptimisationResult::Satisfiable(solution) => {
            println!(
                "Found satisfiable solution with makespan {}",
                solution.get_integer_value(makespan)
            )
        }
        pumpkin_lib::results::OptimisationResult::Unsatisfiable => {
            println!("Unsatisfiable")
        }
        pumpkin_lib::results::OptimisationResult::Unknown => {
            println!("Unknown")
        }
    }
    Ok(())
}
