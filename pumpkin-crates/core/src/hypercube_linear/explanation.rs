use std::fmt::Display;
use std::num::NonZero;

use crate::engine::Assignments;
use crate::engine::VariableNames;
use crate::hypercube_linear::Hypercube;
use crate::hypercube_linear::InconsistentHypercube;
use crate::hypercube_linear::LinearInequality;
use crate::predicate;
use crate::predicates::Predicate;
use crate::variables::IntegerVariable;

/// An explanation of a propagation as a hypercube linear constraint.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HypercubeLinearExplanation {
    pub hypercube: Hypercube,
    pub linear: LinearInequality,
}

impl HypercubeLinearExplanation {
    pub fn nogood(
        predicates: impl IntoIterator<Item = Predicate>,
    ) -> Result<Self, InconsistentHypercube> {
        Ok(HypercubeLinearExplanation {
            hypercube: Hypercube::new(predicates)?,
            linear: LinearInequality::trivially_false(),
        })
    }

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
    pub fn weaken(self, predicate: Predicate) -> HypercubeLinearExplanation {
        let domain = predicate.get_domain();
        let value = predicate.get_right_hand_side();

        let Some(term_to_weaken) = self.linear.term_for_domain(domain) else {
            return self;
        };

        let rhs_delta = if predicate.is_lower_bound_predicate()
            && term_to_weaken.weight.is_positive()
            || predicate.is_upper_bound_predicate() && term_to_weaken.weight.is_negative()
        {
            term_to_weaken
                .weight
                .get()
                .checked_mul(value)
                .expect("integer overflow")
        } else {
            return self;
        };

        let hypercube = self
            .hypercube
            .with_predicate(predicate)
            .expect("weakening causes inconsistent hypercube");

        // Filtering keeps the sorted order of the linear terms.
        let terms = self
            .linear
            .terms()
            .filter(|&term| term != term_to_weaken)
            .map(|term| (term.weight, term.domain));

        let bound = self.linear.bound() - rhs_delta;

        let linear = LinearInequality::new(terms, bound)
            .expect("linear is not trivially satisfiable after weakening");

        HypercubeLinearExplanation { hypercube, linear }
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
}

struct HLDisplay<'expl, 'names> {
    explanation: &'expl HypercubeLinearExplanation,
    variable_names: &'names VariableNames,
}

impl Display for HLDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.explanation.hypercube.display(self.variable_names),
            self.explanation.linear.display(self.variable_names),
        )
    }
}
