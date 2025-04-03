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
use crate::variables::DomainId;
use crate::variables::Literal;

#[derive(Default, Debug)]
pub(crate) struct ProofLiterals {
    /// All the predicates seen in the proof log.
    ///
    /// The predicates in the map are only LessThanEqual or Equal. The other variants are negations
    /// of the predicates in the map.
    variables: HashMap<Predicate, NonZeroU32>,

    /// Maps the domain id of a 0-1 integer `x` to the predicate `p` that it reifies:
    /// `[x == 1] <-> p`.
    ///
    /// Used in substituting the reification domain with the predicate when logging reasons.
    reification_domains: HashMap<DomainId, Predicate>,
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

    /// Given a literal, whenever it shows up in a proof step, substitute it with the provided
    /// predicate.
    pub(crate) fn reify_predicate(&mut self, literal: Literal, predicate: Predicate) {
        // Note: This only works because we assume `literal` is a fresh literal and we are given
        // the positive polarity. That assumption holds as the only place this can be called is
        // transitively through `new_literal_for_predicate`. As soon as this assumption is
        // violated, all hell will break loose.
        let domain = literal.get_true_predicate().get_domain();

        let _ = self.reification_domains.insert(domain, predicate);
    }

    /// The given predicate is a predicate over a literal. This function gets the associated
    /// predicate that was reified with [`Self::reify_predicate`] if it exists.
    fn get_underlying_predicate(&self, predicate: Predicate) -> Option<Predicate> {
        let domain_id = predicate.get_domain();
        let rhs = predicate.get_right_hand_side();

        self.reification_domains
            .get(&domain_id)
            .map(|&reified_predicate| {
                assert!(rhs == 0 || rhs == 1);

                match predicate {
                    // The `predicate` is false
                    Predicate::UpperBound { upper_bound: 0, .. }
                    | Predicate::Equal {
                        equality_constant: 0,
                        ..
                    }
                    | Predicate::NotEqual {
                        not_equal_constant: 1,
                        ..
                    } => !reified_predicate,

                    // The `predicate` is true
                    Predicate::LowerBound { lower_bound: 1, .. }
                    | Predicate::Equal {
                        equality_constant: 1,
                        ..
                    }
                    | Predicate::NotEqual {
                        not_equal_constant: 0,
                        ..
                    } => reified_predicate,

                    p => panic!("{p:?} is not a valid reification predicate"),
                }
            })
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
        // Determine whether `literal` is a reification of another predicate.
        let literal = self.get_underlying_predicate(literal).unwrap_or(literal);

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
