mod ast;
mod compiler;
mod error;
mod instance;
mod parser;

use std::fs::File;
use std::io::Read;
use std::path::Path;

pub use error::*;
use pumpkin_lib::basic_types::CSPSolverExecutionFlag;
use pumpkin_lib::engine::ConstraintSatisfactionSolver;

use crate::flatzinc::instance::FlatZincInstance;
use crate::flatzinc::instance::Output;

const MSG_UNKNOWN: &str = "=====UNKNOWN=====";
const MSG_UNSATISFIABLE: &str = "=====UNSATISFIABLE=====";

pub fn solve(
    mut solver: ConstraintSatisfactionSolver,
    instance: impl AsRef<Path>,
) -> Result<(), FlatZincError> {
    let instance = File::open(instance)?;

    let instance = parse_and_compile(&mut solver, instance)?;

    match solver.solve(i64::MAX) {
        CSPSolverExecutionFlag::Feasible => print_solution(&solver, &instance),
        CSPSolverExecutionFlag::Infeasible => println!("{MSG_UNSATISFIABLE}"),
        CSPSolverExecutionFlag::Timeout => println!("{MSG_UNKNOWN}"),
    }

    Ok(())
}

fn parse_and_compile(
    solver: &mut ConstraintSatisfactionSolver,
    instance: impl Read,
) -> Result<FlatZincInstance, FlatZincError> {
    let ast = parser::parse(instance)?;
    compiler::compile(ast, solver)
}

fn print_solution(solver: &ConstraintSatisfactionSolver, instance: &FlatZincInstance) {
    for output_specification in instance.outputs() {
        match output_specification {
            Output::Bool(output) => output.print_value(|literal| {
                solver
                    .get_propositional_assignments()
                    .is_literal_assigned_true(*literal)
            }),

            Output::Int(output) => output.print_value(|domain_id| {
                solver.get_integer_assignments().get_lower_bound(*domain_id)
            }),

            Output::ArrayOfBool(output) => output.print_value(|literal| {
                solver
                    .get_propositional_assignments()
                    .is_literal_assigned_true(*literal)
            }),

            Output::ArrayOfInt(output) => output.print_value(|domain_id| {
                solver.get_integer_assignments().get_lower_bound(*domain_id)
            }),
        }
    }

    println!("----------");
}

#[cfg(test)]
mod tests {
    use pumpkin_lib::basic_types::DomainId;
    use pumpkin_lib::basic_types::Literal;
    use pumpkin_lib::basic_types::PropositionalVariable;

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

        parse_and_compile(&mut solver, instance.as_bytes()).expect("compilation should succeed");

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

        parse_and_compile(&mut solver, instance.as_bytes()).expect("compilation should succeed");

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

        parse_and_compile(&mut solver, instance.as_bytes()).expect("compilation should succeed");

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
