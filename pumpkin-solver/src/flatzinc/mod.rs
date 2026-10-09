//! Parses a FlatZinc instance and compiles it into a [`Solver`].

mod ast;
mod compiler;
mod error;
mod instance;
mod parser;

use std::io::Read;

pub use error::FlatZincError;
pub use instance::ArrayOutput;
pub use instance::FlatZincInstance;
pub use instance::FlatzincObjective;
pub use instance::Output;
pub use instance::VariableOutput;
use pumpkin_propagators::cumulative::options::CumulativeOptions;

use crate::Solver;

/// The minimum value of an unbounded integer.
///
/// This can be overridden by setting specific bounds under this threshold but this should be done
/// with care since it might lead to overflow/underflow!
pub const MIN_INTEGER_VALUE: i32 = -500_000_000;
/// The maximum value of an unbounded integer.
///
/// This can be overridden by setting specific bounds over this threshold but this should be done
/// with care since it might lead to overflow/underflow!
pub const MAX_INTEGER_VALUE: i32 = 500_000_000;

/// The options that influence how a FlatZinc instance is compiled.
#[derive(Debug, Clone, Copy, Default)]
pub struct CompilationOptions {
    /// Options used for the cumulative constraint (see [`crate::cumulative`]).
    pub cumulative_options: CumulativeOptions,

    /// A full proof needs a constraint tag for every inference, so the equivalences between
    /// variables are then not merged.
    pub is_logging_full_proof: bool,
}

/// Parses the FlatZinc instance in `source` and adds its variables and constraints to `solver`.
pub fn parse_and_compile(
    solver: &mut Solver,
    source: impl Read,
    options: CompilationOptions,
) -> Result<FlatZincInstance, FlatZincError> {
    let ast = parser::parse(source)?;
    compiler::compile(ast, solver, options)
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: The following tests rely on observing the interal state of the solver. This is not good
    // design, and these tests should be re-done.
    //
    // #[test]
    // fn single_bool_gets_compiled_to_literal() {
    //     let model = r#"
    //         var bool: SomeVar;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let starting_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     let _ =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let final_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     assert_eq!(1, final_variables - starting_variables);
    // }

    // #[test]
    // fn output_annotation_is_interpreted_on_bools() {
    //     let model = r#"
    //         var bool: SomeVar ::output_var;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let instance =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let literal = Literal::new(
    //         PropositionalVariable::new(
    //             solver
    //                 .get_propositional_assignments()
    //                 .num_propositional_variables()
    //                 - 1,
    //         ),
    //         true,
    //     );

    //     let outputs = instance.outputs().collect::<Vec<_>>();
    //     assert_eq!(1, outputs.len());

    //     let output = outputs[0].clone();
    //     assert_eq!(output, Output::bool("SomeVar".into(), literal));
    // }

    // #[test]
    // fn equivalent_bools_refer_to_the_same_literal() {
    //     let model = r#"
    //         var bool: SomeVar;
    //         var bool: OtherVar = SomeVar;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let starting_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     let _ =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let final_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     assert_eq!(1, final_variables - starting_variables);
    // }

    // #[test]
    // fn bool_equivalent_to_true_uses_builtin_true_literal() {
    //     let model = r#"
    //         var bool: SomeVar = true;
    //         solve satisfy;
    //     "#;

    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let starting_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     let _ =
    //         parse_and_compile(&mut solver, model.as_bytes()).expect("compilation should
    // succeed");

    //     let final_variables = solver
    //         .get_propositional_assignments()
    //         .num_propositional_variables();

    //     assert_eq!(0, final_variables - starting_variables);
    // }

    // #[test]
    // fn single_variable_gets_compiled_to_domain_id() {
    //     let instance = "var 1..5: SomeVar;\nsolve satisfy;";
    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let _ = parse_and_compile(&mut solver, instance.as_bytes())
    //         .expect("compilation should succeed");

    //     let domains = solver
    //         .get_integer_assignments()
    //         .get_domains()
    //         .collect::<Vec<DomainId>>();

    //     assert_eq!(1, domains.len());

    //     let domain = domains[0];
    //     assert_eq!(1, solver.get_integer_assignments().get_lower_bound(domain));
    //     assert_eq!(5, solver.get_integer_assignments().get_upper_bound(domain));
    // }

    // #[test]
    // fn equal_integer_variables_use_one_domain_id() {
    //     let instance = r#"
    //          var 1..10: SomeVar;
    //          var 0..11: OtherVar = SomeVar;
    //          solve satisfy;
    //      "#;
    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let _ = parse_and_compile(&mut solver, instance.as_bytes())
    //         .expect("compilation should succeed");

    //     let domains = solver
    //         .get_integer_assignments()
    //         .get_domains()
    //         .collect::<Vec<DomainId>>();

    //     assert_eq!(1, domains.len());

    //     let domain = domains[0];
    //     assert_eq!(1, solver.get_integer_assignments().get_lower_bound(domain));
    //     assert_eq!(10, solver.get_integer_assignments().get_upper_bound(domain));
    // }

    // #[test]
    // fn var_equal_to_constant_reuse_domain_id() {
    //     let instance = r#"
    //          var 1..10: SomeVar = 5;
    //          var 0..11: OtherVar = 5;
    //          solve satisfy;
    //      "#;
    //     let mut solver = ConstraintSatisfactionSolver::default();

    //     let _ = parse_and_compile(&mut solver, instance.as_bytes())
    //         .expect("compilation should succeed");

    //     let domains = solver
    //         .get_integer_assignments()
    //         .get_domains()
    //         .collect::<Vec<DomainId>>();

    //     assert_eq!(1, domains.len());

    //     let domain = domains[0];
    //     assert_eq!(5, solver.get_integer_assignments().get_lower_bound(domain));
    //     assert_eq!(5, solver.get_integer_assignments().get_upper_bound(domain));
    // }

    #[test]
    fn array_1d_of_boolean_variables() {
        let instance = r#"
            var bool: x1;
            var bool: x2;
            array [1..2] of var bool: xs :: output_array([1..2]) = [x1,x2];
            solve satisfy;
        "#;
        let mut solver = Solver::default();

        let instance = parse_and_compile(
            &mut solver,
            instance.as_bytes(),
            CompilationOptions::default(),
        )
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
        let mut solver = Solver::default();

        let instance = parse_and_compile(
            &mut solver,
            instance.as_bytes(),
            CompilationOptions::default(),
        )
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
        let mut solver = Solver::default();

        let instance = parse_and_compile(
            &mut solver,
            instance.as_bytes(),
            CompilationOptions::default(),
        )
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
        let mut solver = Solver::default();

        let instance = parse_and_compile(
            &mut solver,
            instance.as_bytes(),
            CompilationOptions::default(),
        )
        .expect("compilation should succeed");

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }
}
