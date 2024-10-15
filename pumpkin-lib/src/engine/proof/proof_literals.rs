use std::io::Write;
use std::num::NonZeroI32;
use std::num::NonZeroU32;

use drcp_format::writer::AtomicConstraint;
use drcp_format::writer::Comparison;
use drcp_format::writer::IntAtomicConstraint;
use drcp_format::writer::LiteralCodeProvider;
use drcp_format::writer::LiteralDefinitions;

use crate::basic_types::KeyedVec;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::Literal;
use crate::engine::variables::PropositionalVariable;
use crate::engine::VariableLiteralMappings;
use crate::variable_names::VariableNames;

#[derive(Debug)]
pub(crate) struct ProofLiterals {
    /// All the variables seen in the proof log.
    variables: KeyedVec<PropositionalVariable, Option<NonZeroU32>>,
    /// The next code that can be used when a new variable is encountered.
    next_code: NonZeroU32,
}

impl Default for ProofLiterals {
    fn default() -> Self {
        ProofLiterals {
            variables: KeyedVec::default(),
            next_code: NonZeroU32::new(1).unwrap(),
        }
    }
}

impl ProofLiterals {
    pub(crate) fn write(
        self,
        sink: impl Write,
        variable_names: &VariableNames,
        variable_literal_mapping: &VariableLiteralMappings,
    ) -> std::io::Result<()> {
        let entries = self
            .variables
            .into_entries()
            .filter_map(|(variable, code)| code.map(|c| (variable, c)));

        let mut definitions = LiteralDefinitions::default();

        for (variable, code) in entries {
            let predicates = variable_literal_mapping.get_predicates(Literal::new(variable, true));

            let atomics = variable_names
                .get_propositional_name(variable)
                .into_iter()
                .map(AtomicConstraint::Bool)
                .chain(
                    predicates
                        .map(|predicate| integer_predicate_to_atomic(predicate, variable_names)),
                );

            for atomic in atomics {
                definitions.add(code, atomic);
            }
        }

        definitions.write(sink)
    }

    fn get_next_code(&mut self) -> NonZeroU32 {
        let code = self.next_code;
        self.next_code = self
            .next_code
            .checked_add(1)
            .expect("fewer than i32::MAX literals");
        code
    }
}

fn integer_predicate_to_atomic(
    predicate: IntegerPredicate,
    variable_names: &VariableNames,
) -> AtomicConstraint<'_> {
    match predicate {
        IntegerPredicate::LowerBound {
            domain_id,
            lower_bound,
        } => AtomicConstraint::Int(IntAtomicConstraint {
            name: variable_names
                .get_int_name(domain_id)
                .expect("integer domain is unnamed"),
            comparison: Comparison::GreaterThanEqual,
            value: lower_bound.into(),
        }),
        IntegerPredicate::UpperBound {
            domain_id,
            upper_bound,
        } => AtomicConstraint::Int(IntAtomicConstraint {
            name: variable_names
                .get_int_name(domain_id)
                .expect("integer domain is unnamed"),
            comparison: Comparison::LessThanEqual,
            value: upper_bound.into(),
        }),
        IntegerPredicate::NotEqual {
            domain_id,
            not_equal_constant,
        } => AtomicConstraint::Int(IntAtomicConstraint {
            name: variable_names
                .get_int_name(domain_id)
                .expect("integer domain is unnamed"),
            comparison: Comparison::NotEqual,
            value: not_equal_constant.into(),
        }),
        IntegerPredicate::Equal {
            domain_id,
            equality_constant,
        } => AtomicConstraint::Int(IntAtomicConstraint {
            name: variable_names
                .get_int_name(domain_id)
                .expect("integer domain is unnamed"),
            comparison: Comparison::Equal,
            value: equality_constant.into(),
        }),
    }
}

impl LiteralCodeProvider for ProofLiterals {
    type Literal = Literal;

    fn to_code(&mut self, literal: Self::Literal) -> NonZeroI32 {
        let variable = literal.get_propositional_variable();

        self.variables.accomodate(variable, None);

        let variable_code = if let Some(code) = self.variables[variable] {
            code
        } else {
            let code = self.get_next_code();
            self.variables[variable] = Some(code);

            code
        };

        let code: NonZeroI32 = variable_code
            .try_into()
            .expect("fewer than i32::MAX literals");

        if literal.is_positive() {
            code
        } else {
            -code
        }
    }
}
