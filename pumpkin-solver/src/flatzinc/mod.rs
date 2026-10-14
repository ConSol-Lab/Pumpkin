//! Parses a FlatZinc instance and compiles it into a [`Solver`]. The compiler is shared by the
//! solver binary and the proof processor, which posts the same propagators.

mod ast;
mod compiler;
mod error;
mod instance;
mod parser;
pub mod preamble;

use std::io::Read;

pub use error::FlatZincError;
pub use instance::ArrayOutput;
pub use instance::FlatZincInstance;
pub use instance::FlatzincObjective;
pub use instance::Output;
pub use instance::VariableOutput;
use pumpkin_propagators::cumulative::options::CumulativeOptions;

pub use self::compiler::ViewStatistics;
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
#[derive(Debug, Clone, Default)]
pub struct CompilationOptions {
    /// Options used for the cumulative constraint (see [`crate::cumulative`]).
    pub cumulative_options: CumulativeOptions,

    /// The proof that is logged, which decides the preprocessing the compiler may do.
    pub proof_logging: ProofLogging,

    /// Whether all solutions are enumerated, which the passes that remove solutions refuse.
    pub all_solutions: bool,

    /// Stop after compiling: write the proof steps that state the views, in the format of
    /// fzn-drcp-check, to `head_path`, report what the view pass did, and do not solve.
    pub preprocess_only: bool,
    pub head_path: Option<std::path::PathBuf>,
    /// State views as declared replacements rather than rewriting every constraint.
    pub declare_replacements: bool,
    /// Make a boolean that a reified comparison with a constant defines the literal of the
    /// comparison, when it occurs only as a literal.
    pub literal_predicates: bool,

    /// Detect interchangeable variables and post lex-leader constraints to break the
    /// symmetry. Refused when enumerating all solutions and under proof logging other than a
    /// croc block proof or croc scaffold, where each lex-leader constraint is justified by a
    /// lex-leader step.
    pub symmetry_breaking: bool,

    /// Detect dominated variables in linear models and post dominance-breaking constraints.
    /// Refused when enumerating all solutions, with symmetry breaking, and under proof logging
    /// other than a croc block proof or croc scaffold, where each posted constraint is
    /// justified.
    pub dominance_breaking: bool,

    /// Fix variables to a bound when moving them towards it cannot violate a constraint or
    /// worsen the objective. Refused when enumerating all solutions, with symmetry breaking, and
    /// under proof logging other than a croc block proof or croc scaffold, where each fixing is
    /// justified.
    pub dual_fixing: bool,

    /// Detect interchangeable values and post value-precedence constraints. Refused when
    /// enumerating all solutions, with the other breaking options, and under proof logging other
    /// than a croc block proof or croc scaffold, where the exchanges are stated as value symmetry
    /// steps and nothing is posted.
    pub value_symmetry_breaking: bool,
}

impl CompilationOptions {
    /// Whether a full DRCP proof is logged.
    pub fn is_logging_full_proof(&self) -> bool {
        matches!(
            self.proof_logging,
            ProofLogging::Drcp {
                proof_type: DrcpProofType::Full
            }
        )
    }

    /// Whether the proof, if any, states symmetry and dominance breaking steps: a croc proof in
    /// the block format, or a croc scaffold, whose preamble states them.
    pub(crate) fn proof_states_breaking_steps(&self) -> bool {
        !self.proof_logging.is_drcp()
    }
}

/// The proof that is logged, in its format and with the options of that format.
#[derive(Default, Clone, Debug, PartialEq, Eq)]
#[allow(clippy::large_enum_variant, reason = "there is one per compilation")]
#[allow(variant_size_differences, reason = "there is one per compilation")]
pub enum ProofLogging {
    /// No proof is logged.
    #[default]
    Disabled,
    /// A proof in the DRCP format.
    Drcp { proof_type: DrcpProofType },
    /// A proof in the croc format of fzn-croc-check (experimental).
    Croc {
        proof_type: CrocProofType,
        /// The preamble of a croc scaffold, whose preprocessing the compiler applies instead of
        /// deciding it: the views, constants and literals it gives are made, the constraints it
        /// skips are not posted, and its steps head the proof. Used by the proof processor,
        /// which reads a scaffold over the variables of the solver that wrote the preamble.
        applied_preamble: Option<preamble::Preamble>,
    },
}

impl ProofLogging {
    /// Whether the proof is in the DRCP format, for which the compiler makes no views and posts
    /// the terms of each constraint as the model writes them, as the DRCP checker reads them.
    pub(crate) fn is_drcp(&self) -> bool {
        matches!(self, ProofLogging::Drcp { .. })
    }
}

/// The type of a DRCP proof.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DrcpProofType {
    /// Only the proof scaffold.
    Scaffold,
    /// The full proof with hints.
    Full,
}

/// The type of a croc proof.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CrocProofType {
    /// The proof scaffold, headed by the preamble that states the compiler's preprocessing, for
    /// the proof processor.
    Scaffold,
    /// The full proof in the block format.
    Blocks,
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

    #[test]
    fn variable_alias_compiles_without_proof_logging() {
        let instance = r#"
            var 1..10: a :: output_var;
            var 1..10: b :: output_var = a;
            solve satisfy;
        "#;
        let mut solver = Solver::default();

        let instance = parse_and_compile(
            &mut solver,
            instance.as_bytes(),
            CompilationOptions::default(),
        )
        .expect("compilation should succeed");

        assert_eq!(2, instance.outputs().count());
    }
}
