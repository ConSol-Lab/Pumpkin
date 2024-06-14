mod ast;
mod compiler;
pub(crate) mod error;
mod instance;
mod minizinc_optimiser;
mod parser;

use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::time::Duration;

use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
use pumpkin_lib::basic_types::SolutionReference;
use pumpkin_lib::basic_types::Stopwatch;
use pumpkin_lib::branching::Brancher;
use pumpkin_lib::branching::DynamicBrancher;
use pumpkin_lib::branching::IndependentVariableValueBrancher;
use pumpkin_lib::engine::variables::Literal;
use pumpkin_lib::engine::AssignmentsInteger;
use pumpkin_lib::engine::AssignmentsPropositional;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;
use pumpkin_lib::optimisation::log_statistics;
use pumpkin_lib::optimisation::log_statistics_with_objective;
use pumpkin_lib::pumpkin_assert_moderate;
use pumpkin_lib::pumpkin_assert_simple;

use self::instance::FlatZincInstance;
use self::instance::Output;
use self::minizinc_optimiser::MinizincOptimisationResult;
use self::minizinc_optimiser::MinizincOptimiser;
use crate::flatzinc::error::FlatZincError;

const MSG_UNKNOWN: &str = "=====UNKNOWN=====";
const MSG_UNSATISFIABLE: &str = "=====UNSATISFIABLE=====";

#[derive(Debug, Clone, Copy)]
pub(crate) struct FlatZincOptions {
    /// If `true`, the solver will not strictly keep to the search annotations in the flatzinc.
    pub(crate) free_search: bool,

    /// For satisfaction problems, print all solutions. For optimisation problems, this instructs
    /// the solver to print intermediate solutions.
    pub(crate) all_solutions: bool,
}

pub(crate) fn solve(
    mut solver: ConstraintSatisfactionSolver,
    instance: impl AsRef<Path>,
    time_limit: Option<Duration>,
    options: FlatZincOptions,
) -> Result<(), FlatZincError> {
    let instance = File::open(instance)?;

    let instance = parse_and_compile(&mut solver, instance)?;
    let outputs = instance.outputs.clone();

    let value = if let Some(objective_function) = &instance.objective_function {
        let brancher = if options.free_search {
            // The free search flag is active, we just use the default brancher
            DynamicBrancher::new(vec![Box::new(
                IndependentVariableValueBrancher::default_over_all_propositional_variables(&solver),
            )])
        } else {
            instance.search.expect("Expected a search to be defined")
        };

        let mut optimisation_solver = MinizincOptimiser::new(&mut solver, *objective_function);
        match optimisation_solver.solve(time_limit, brancher, &instance.outputs) {
            MinizincOptimisationResult::Optimal {
                optimal_objective_value,
            } => {
                println!("==========");
                Some(optimal_objective_value)
            }
            MinizincOptimisationResult::Satisfiable {
                best_found_objective_value,
            } => Some(best_found_objective_value),
            MinizincOptimisationResult::Infeasible => {
                println!("{MSG_UNSATISFIABLE}");
                None
            }
            MinizincOptimisationResult::Unknown => {
                println!("{MSG_UNKNOWN}");
                None
            }
        }
    } else {
        let stopwatch = Stopwatch::new(
            time_limit
                .map(|limit| limit.as_secs() as i64)
                .unwrap_or(i64::MAX),
        );

        let mut brancher = instance.search.expect("Expected a search to be defined");

        let mut found_solution = false;

        loop {
            match solver.solve(stopwatch.get_remaining_time_budget(), &mut brancher) {
                CSPSolverExecutionFlag::Feasible => {
                    found_solution = true;

                    print_solution_from_solver(
                        solver.get_integer_assignments(),
                        solver.get_propositional_assignments(),
                        &outputs,
                    );

                    brancher.on_solution(SolutionReference::new(
                        solver.get_propositional_assignments(),
                        solver.get_integer_assignments(),
                    ));

                    let could_find_another_solution =
                        add_blocking_clause(&mut solver, &outputs, &mut brancher);

                    if !could_find_another_solution {
                        println!("==========");
                        break;
                    }
                }
                CSPSolverExecutionFlag::Infeasible if !found_solution => {
                    println!("{MSG_UNSATISFIABLE}");
                    break;
                }
                CSPSolverExecutionFlag::Infeasible => {
                    println!("==========");
                    break;
                }

                // Only when no solutions are found should the UNKNOWN marker be printed.
                CSPSolverExecutionFlag::Timeout if !found_solution => {
                    println!("{MSG_UNKNOWN}");
                    break;
                }
                CSPSolverExecutionFlag::Timeout => break,
            }

            if !options.all_solutions {
                break;
            }
        }

        None
    };

    if let Some(value) = value {
        log_statistics_with_objective(&solver, value)
    } else {
        log_statistics(&solver)
    }

    Ok(())
}

/// Creates a clause which prevents the current solution from occurring again by going over the
/// defined output variables and creating a clause which prevents those values from being assigned.
///
/// This method is used when attempting to find multiple solutions. It restores the state of the
/// passed [`ConstraintSatisfactionSolver`] to the root (using
/// [`ConstraintSatisfactionSolver::restore_state_at_root`]) and returns true if adding the clause
/// was successful (i.e. it is possible that there could be another solution) and returns false
/// otherwise (i.e. if adding a clause led to a conflict which indicates that there are no more
/// solutions).
fn add_blocking_clause(
    solver: &mut ConstraintSatisfactionSolver,
    outputs: &[Output],
    brancher: &mut impl Brancher,
) -> bool {
    let clause = outputs
        .iter()
        .flat_map(|output| match output {
            Output::Bool(bool) => {
                pumpkin_assert_moderate!(solver
                    .get_propositional_assignments()
                    .is_literal_assigned(*bool.get_variable()));
                let value = solver
                    .get_propositional_assignments()
                    .is_variable_assigned_true(bool.get_variable().get_propositional_variable());
                Box::new(std::iter::once(Literal::new(
                    bool.get_variable().get_propositional_variable(),
                    value,
                )))
            }
            Output::Int(int) => {
                let value = solver
                    .get_integer_assignments()
                    .get_assigned_value(*int.get_variable());
                Box::new(std::iter::once(
                    solver.get_equality_literal(*int.get_variable(), value),
                ))
            }
            Output::ArrayOfBool(array_of_bool) => {
                let literals: Box<dyn Iterator<Item = Literal>> =
                    Box::new(array_of_bool.get_contents().map(|literal| {
                        pumpkin_assert_moderate!(solver
                            .get_propositional_assignments()
                            .is_literal_assigned(*literal));
                        let value = solver
                            .get_propositional_assignments()
                            .is_variable_assigned_true(literal.get_propositional_variable());
                        Literal::new(literal.get_propositional_variable(), value)
                    }));
                literals
            }
            Output::ArrayOfInt(array_of_ints) => {
                Box::new(array_of_ints.get_contents().map(|int| {
                    let value = solver.get_integer_assignments().get_assigned_value(*int);
                    solver.get_equality_literal(*int, value)
                }))
            }
        })
        .map(|literal| !literal)
        .collect::<Vec<_>>();
    solver.restore_state_at_root(brancher);
    if clause.is_empty() {
        return false;
    }
    solver.add_clause(clause).is_ok()
}

fn parse_and_compile(
    solver: &mut ConstraintSatisfactionSolver,
    instance: impl Read,
) -> Result<FlatZincInstance, FlatZincError> {
    let ast = parser::parse(instance)?;
    compiler::compile(ast, solver)
}

/// Prints the current solution stored in the provided [`AssignmentsInteger`] and
/// [`AssignmentsPropositional`].
fn print_solution_from_solver(
    assignments_integer: &AssignmentsInteger,
    assignments_propositional: &AssignmentsPropositional,
    outputs: &[Output],
) {
    for output_specification in outputs {
        match output_specification {
            Output::Bool(output) => output.print_value(|literal| {
                pumpkin_assert_simple!(assignments_propositional.is_literal_assigned(*literal));
                assignments_propositional.is_literal_assigned_true(*literal)
            }),

            Output::Int(output) => {
                output.print_value(|domain_id| assignments_integer.get_assigned_value(*domain_id))
            }

            Output::ArrayOfBool(output) => output.print_value(|literal| {
                pumpkin_assert_simple!(assignments_propositional.is_literal_assigned(*literal));
                assignments_propositional.is_literal_assigned_true(*literal)
            }),

            Output::ArrayOfInt(output) => {
                output.print_value(|domain_id| assignments_integer.get_assigned_value(*domain_id))
            }
        }
    }

    println!("----------");
}

#[cfg(test)]
mod tests {
    use pumpkin_lib::engine::variables::DomainId;
    use pumpkin_lib::engine::variables::Literal;
    use pumpkin_lib::engine::variables::PropositionalVariable;

    use super::*;

    #[test]
    fn single_bool_gets_compiled_to_literal() {
        let model = r#"
            var bool: SomeVar;
            solve satisfy;
        "#;

        let mut solver = ConstraintSatisfactionSolver::default();

        let starting_variables = solver
            .get_propositional_assignments()
            .num_propositional_variables();

        let _ =
            parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should succeed");

        let final_variables = solver
            .get_propositional_assignments()
            .num_propositional_variables();

        assert_eq!(1, final_variables - starting_variables);
    }

    #[test]
    fn output_annotation_is_interpreted_on_bools() {
        let model = r#"
            var bool: SomeVar ::output_var;
            solve satisfy;
        "#;

        let mut solver = ConstraintSatisfactionSolver::default();

        let instance =
            parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should succeed");

        let literal = Literal::new(
            PropositionalVariable::new(
                solver
                    .get_propositional_assignments()
                    .num_propositional_variables()
                    - 1,
            ),
            true,
        );

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());

        let output = outputs[0].clone();
        assert_eq!(output, Output::bool("SomeVar".into(), literal));
    }

    #[test]
    fn equivalent_bools_refer_to_the_same_literal() {
        let model = r#"
            var bool: SomeVar;
            var bool: OtherVar = SomeVar;
            solve satisfy;
        "#;

        let mut solver = ConstraintSatisfactionSolver::default();

        let starting_variables = solver
            .get_propositional_assignments()
            .num_propositional_variables();

        let _ =
            parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should succeed");

        let final_variables = solver
            .get_propositional_assignments()
            .num_propositional_variables();

        assert_eq!(1, final_variables - starting_variables);
    }

    #[test]
    fn bool_equivalent_to_true_uses_builtin_true_literal() {
        let model = r#"
            var bool: SomeVar = true;
            solve satisfy;
        "#;

        let mut solver = ConstraintSatisfactionSolver::default();

        let starting_variables = solver
            .get_propositional_assignments()
            .num_propositional_variables();

        let _ =
            parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should succeed");

        let final_variables = solver
            .get_propositional_assignments()
            .num_propositional_variables();

        assert_eq!(0, final_variables - starting_variables);
    }

    #[test]
    fn single_variable_gets_compiled_to_domain_id() {
        let instance = "var 1..5: SomeVar;\nsolve satisfy;";
        let mut solver = ConstraintSatisfactionSolver::default();

        let _ = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let domains = solver
            .get_integer_assignments()
            .get_domains()
            .collect::<Vec<DomainId>>();

        assert_eq!(1, domains.len());

        let domain = domains[0];
        assert_eq!(1, solver.get_integer_assignments().get_lower_bound(domain));
        assert_eq!(5, solver.get_integer_assignments().get_upper_bound(domain));
    }

    #[test]
    fn equal_integer_variables_use_one_domain_id() {
        let instance = r#"
             var 1..10: SomeVar;
             var 0..11: OtherVar = SomeVar;
             solve satisfy;
         "#;
        let mut solver = ConstraintSatisfactionSolver::default();

        let _ = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let domains = solver
            .get_integer_assignments()
            .get_domains()
            .collect::<Vec<DomainId>>();

        assert_eq!(1, domains.len());

        let domain = domains[0];
        assert_eq!(1, solver.get_integer_assignments().get_lower_bound(domain));
        assert_eq!(10, solver.get_integer_assignments().get_upper_bound(domain));
    }

    #[test]
    fn var_equal_to_constant_reuse_domain_id() {
        let instance = r#"
             var 1..10: SomeVar = 5;
             var 0..11: OtherVar = 5;
             solve satisfy;
         "#;
        let mut solver = ConstraintSatisfactionSolver::default();

        let _ = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let domains = solver
            .get_integer_assignments()
            .get_domains()
            .collect::<Vec<DomainId>>();

        assert_eq!(1, domains.len());

        let domain = domains[0];
        assert_eq!(5, solver.get_integer_assignments().get_lower_bound(domain));
        assert_eq!(5, solver.get_integer_assignments().get_upper_bound(domain));
    }

    #[test]
    fn array_1d_of_boolean_variables() {
        let instance = r#"
            var bool: x1;
            var bool: x2;
            array [1..2] of var bool: xs :: output_array([1..2]) = [x1,x2];
            solve satisfy;
        "#;
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());

        assert!(matches!(outputs[0], Output::ArrayOfBool(_)));
    }

    #[test]
    fn array_2d_of_boolean_variables() {
        let instance = r#"
            var bool: x1;
            var bool: x2;
            var bool: x3;
            var bool: x4;
            array [1..4] of var bool: xs :: output_array([1..2, 1..2]) = [x1,x2,x3,x4];
            solve satisfy;
        "#;
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }

    #[test]
    fn array_1d_of_integer_variables() {
        let instance = r#"
            var 1..10: x1;
            var 1..10: x2;
            array [1..2] of var int: xs :: output_array([1..2]) = [x1,x2];
            solve satisfy;
        "#;
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());

        assert!(matches!(outputs[0], Output::ArrayOfInt(_)));
    }

    #[test]
    fn array_2d_of_integer_variables() {
        let instance = r#"
            var 1..10: x1;
            var 1..10: x2;
            var 1..10: x3;
            var 1..10: x4;
            array [1..4] of var 1..10: xs :: output_array([1..2, 1..2]) = [x1,x2,x3,x4];
            solve satisfy;
        "#;
        let mut solver = ConstraintSatisfactionSolver::default();

        let instance = parse_and_compile(&mut solver, instance.as_bytes())
            .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }
}
