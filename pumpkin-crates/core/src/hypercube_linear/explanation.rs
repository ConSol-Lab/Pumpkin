use std::borrow::Cow;
use std::fmt::Display;
use std::num::NonZero;

use crate::engine::Assignments;
use crate::engine::VariableNames;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::LinearInequality;
use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::IntegerVariable;

/// An explanation of a propagation as a hypercube linear constraint.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HypercubeLinearExplanation<'a> {
    pub hypercube: Cow<'a, Hypercube>,
    pub linear: Cow<'a, LinearInequality>,
}

impl<'a> HypercubeLinearExplanation<'a> {
    /// Get the reason set for this hypercube linear.
    ///
    /// Only makes sense if the hypercube linear is conflicting w.r.t. the given assignments.
    pub fn reason_set(&self, assignments: &Assignments) -> impl Iterator<Item = Predicate> {
        self.hypercube
            .iter_predicates()
            .chain(self.linear.terms().map(|term| {
                let term_lb = term.lower_bound(assignments);
                predicate![term >= term_lb]
            }))
    }

    /// Weaken this hypercube linear on the given predicate.
    ///
    /// It may be that weakening on the given predicate does nothing, in which case this is a
    /// no-op.
    pub fn weaken(mut self, predicate: Predicate) -> HypercubeLinearExplanation<'a> {
        let domain = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        let Some(term_to_weaken) = self.linear.term_for_domain(domain) else {
            return self;
        };

        let rhs_delta = if predicate.is_lower_bound_predicate()
            && term_to_weaken.scale.is_positive()
            || predicate.is_upper_bound_predicate() && term_to_weaken.scale.is_negative()
        {
            term_to_weaken
                .scale
                .checked_mul(value)
                .expect("integer overflow")
        } else {
            return self;
        };

        let hypercube = std::mem::take(self.hypercube.to_mut());
        let hypercube = hypercube
            .with_predicate(predicate)
            .expect("weakening causes inconsistent hypercube");

        // Filtering keeps the sorted order of the linear terms.
        let terms = self
            .linear
            .terms()
            .filter(|&term| term != term_to_weaken)
            .map(|view| {
                let weight = NonZero::new(view.scale).expect("affine view scales are non-zero");
                (weight, view.inner)
            });

        let bound = self.linear.bound() - rhs_delta;

        let linear = LinearInequality::new(terms, bound)
            .expect("linear is not trivially satisfiable after weakening");

        HypercubeLinearExplanation {
            hypercube: Cow::Owned(hypercube),
            linear: Cow::Owned(linear),
        }
    }

    /// Write the hypercube linear constraint in a human-friendly way.
    pub(crate) fn display<'this, 'names>(
        &'this self,
        variable_names: &'names VariableNames,
    ) -> impl Display + 'names
    where
        'this: 'names,
        'names: 'this,
    {
        HLDisplay {
            explanation: self,
            variable_names,
        }
    }

    pub fn into_owned(self) -> HypercubeLinearExplanation<'static> {
        HypercubeLinearExplanation {
            hypercube: Cow::Owned(self.hypercube.into_owned()),
            linear: Cow::Owned(self.linear.into_owned()),
        }
    }
}

struct HLDisplay<'data, 'expl, 'names> {
    explanation: &'expl HypercubeLinearExplanation<'data>,
    variable_names: &'names VariableNames,
}

impl Display for HLDisplay<'_, '_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.explanation.hypercube.display(self.variable_names),
            self.explanation.linear.display(self.variable_names),
        )
    }
}
