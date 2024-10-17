use std::io::Write;
use std::num::NonZeroI32;
use std::num::NonZeroU32;

use drcp_format::writer::LiteralCodeProvider;
use drcp_format::AtomicConstraint;
use drcp_format::Comparison;
use drcp_format::IntAtomicConstraint;
use drcp_format::LiteralDefinitions;

use crate::basic_types::HashMap;
use crate::predicates::Predicate;
use crate::variable_names::VariableNames;

#[derive(Default, Debug)]
pub(crate) struct ProofLiterals {
    /// All the predicates seen in the proof log.
    ///
    /// The predicates in the map are only LessThanEqual or Equal. The other variants are negations
    /// of the predicates in the map.
    variables: HashMap<Predicate, NonZeroU32>,
}

impl ProofLiterals {
    pub(crate) fn write(
        self,
        sink: impl Write,
        variable_names: &VariableNames,
    ) -> std::io::Result<()> {
        let mut definitions = LiteralDefinitions::default();

        for (predicate, code) in self.variables.into_iter() {
            let proof_atomic = predicate_to_atomic(predicate, variable_names);
            definitions.add(code, proof_atomic);
        }

        definitions.write(sink)
    }
}

fn predicate_to_atomic(
    predicate: Predicate,
    variable_names: &VariableNames,
) -> AtomicConstraint<&str> {
    match predicate {
        Predicate::UpperBound {
            domain_id,
            upper_bound,
        } => AtomicConstraint::Int(IntAtomicConstraint {
            name: variable_names
                .get_int_name(domain_id)
                .expect("integer domain is unnamed"),
            comparison: Comparison::LessThanEqual,
            value: upper_bound.into(),
        }),
        Predicate::Equal {
            domain_id,
            equality_constant,
        } => AtomicConstraint::Int(IntAtomicConstraint {
            name: variable_names
                .get_int_name(domain_id)
                .expect("integer domain is unnamed"),
            comparison: Comparison::Equal,
            value: equality_constant.into(),
        }),

        Predicate::NotEqual { .. } | Predicate::LowerBound { .. } => {
            panic!("Only Equal and UpperBound predicates should be in the literal definition")
        }
    }
}

impl LiteralCodeProvider for ProofLiterals {
    type Literal = Predicate;

    fn to_code(&mut self, literal: Self::Literal) -> NonZeroI32 {
        let key = match literal {
            l @ (Predicate::UpperBound { .. } | Predicate::Equal { .. }) => l,
            l @ (Predicate::LowerBound { .. } | Predicate::NotEqual { .. }) => !l,
        };

        let next_code = NonZeroU32::new(self.variables.len() as u32 + 1).unwrap();
        let code = *self.variables.entry(key).or_insert(next_code);

        let code: NonZeroI32 = code
            .try_into()
            .expect("cannot handle more than i32::MAX literal codes");

        if key == literal {
            code
        } else {
            -code
        }
    }
}
