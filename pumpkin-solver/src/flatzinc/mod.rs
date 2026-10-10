//! The FlatZinc front end of the solver: parsing a model and compiling it into a [`Solver`],
//! shared by the solver binary and the proof processor, which posts the same propagators.

mod ast;
mod compiler;
pub mod error;
pub mod instance;
mod parser;
pub mod preamble;

use std::io::Read;

#[cfg(doc)]
use pumpkin_constraints::cumulative;
use pumpkin_propagators::cumulative::options::CumulativeOptions;

pub use self::compiler::ViewStatistics;
use self::error::FlatZincError;
use self::instance::FlatZincInstance;
use crate::Solver;
use crate::core::optimisation::OptimisationStrategy;

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

#[derive(Debug, Clone, Default)]
pub struct FlatZincOptions {
    /// If `true`, the solver will not strictly keep to the search annotations in the flatzinc.
    pub free_search: bool,

    /// For satisfaction problems, print all solutions. For optimisation problems, this instructs
    /// the solver to print intermediate solutions.
    pub all_solutions: bool,

    /// Options used for the cumulative constraint (see [`cumulative`]).
    pub cumulative_options: CumulativeOptions,

    /// Determines which type of search is performed by the solver
    pub optimisation_strategy: OptimisationStrategy,

    /// The type of proof that is logged. This influences which preprocessing steps we can do.
    pub proof_type: Option<ProofType>,

    /// The proof format, which decides whether the view pass runs.
    pub proof_format: ProofFormat,

    /// Indicates that the solver should perform verbose logging
    pub verbose: bool,

    /// Write the preamble that states the compiler's preprocessing, for the proof processor, at
    /// the head of the croc scaffold the proof log writes.
    pub writes_preamble: bool,

    /// Stop after compiling: write the proof steps that state the views, in the format of
    /// fzn-drcp-check, to `head_path`, report what the view pass did, and do not solve.
    pub preprocess_only: bool,
    pub head_path: Option<std::path::PathBuf>,
    /// State views as declared replacements rather than rewriting every constraint.
    pub declare_replacements: bool,
}

/// The proof format, which decides what the compiler does for a proof.
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum ProofFormat {
    /// The DRCP format, without the steps it cannot state.
    #[default]
    Drcp,
    /// The croc format of fzn-croc-check, with every step.
    Croc,
}

/// The type of proof that is logged.
#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum ProofType {
    /// Log only the proof scaffold.
    #[default]
    Scaffold,
    /// Log the full proof with hints.
    Full,
    /// Log the full proof in the croc block format (experimental).
    Blocks,
}

/// Parse the FlatZinc model and compile it into the solver.
pub fn parse_and_compile(
    solver: &mut Solver,
    instance: impl Read,
    options: FlatZincOptions,
) -> Result<(FlatZincInstance, ViewStatistics), FlatZincError> {
    let ast = parser::parse(instance)?;
    compiler::compile(ast, solver, options)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::flatzinc::instance::Output;

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

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
                .expect("compilation should succeed")
                .0;

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

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
                .expect("compilation should succeed")
                .0;

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

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
                .expect("compilation should succeed")
                .0;

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

        let instance =
            parse_and_compile(&mut solver, instance.as_bytes(), FlatZincOptions::default())
                .expect("compilation should succeed")
                .0;

        let outputs = instance.outputs().collect::<Vec<_>>();
        assert_eq!(1, outputs.len());
    }
}
