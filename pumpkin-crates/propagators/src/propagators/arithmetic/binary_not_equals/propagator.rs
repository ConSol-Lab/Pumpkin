use pumpkin_core::conjunction;
use pumpkin_core::predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::Domains;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::state::PropagatorConflict;
use pumpkin_core::variables::IntegerVariable;

/// Propagator for the constraint `a != b`.
#[derive(Clone, Debug)]
pub struct BinaryNotEqualsPropagator<AVar, BVar> {
    pub(super) a: AVar,
    pub(super) b: BVar,

    pub(super) inference_code: InferenceCode,
}

impl<AVar, BVar> Propagator for BinaryNotEqualsPropagator<AVar, BVar>
where
    AVar: IntegerVariable + 'static,
    BVar: IntegerVariable + 'static,
{
    fn detect_inconsistency(&self, domains: Domains) -> Option<PropagatorConflict> {
        // We first check whether they are both fixed
        if let Some(fixed_a) = domains.fixed_value(&self.a)
            && let Some(fixed_b) = domains.fixed_value(&self.b)
            && fixed_a == fixed_b
        {
            // If they are, and they are assigned to the same value, then we have detected a
            // conflict
            Some(PropagatorConflict {
                conjunction: conjunction!([self.a == fixed_a] & [self.b == fixed_a]),
                inference_code: self.inference_code.clone(),
            })
        } else {
            None
        }
    }

    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "BinaryNotEq"
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.domains()) {
            return Err(conflict.into());
        }

        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        if a_ub < b_lb || b_ub < a_lb {
            // The domains are non-overlapping
            return Ok(());
        }

        // If `a` is fixed then we can propagate
        if a_lb == a_ub {
            context.post(
                predicate!(self.b != a_lb),
                (conjunction!([self.a == a_lb]), &self.inference_code),
            )?;
        }

        // If `b` is fixed then we can propagate
        if b_lb == b_ub {
            context.post(
                predicate!(self.a != b_lb),
                (conjunction!([self.b == b_lb]), &self.inference_code),
            )?;
        }

        Ok(())
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        if let Some(conflict) = self.detect_inconsistency(context.domains()) {
            return Err(conflict.into());
        }

        let a_lb = context.lower_bound(&self.a);
        let a_ub = context.upper_bound(&self.a);

        let b_lb = context.lower_bound(&self.b);
        let b_ub = context.upper_bound(&self.b);

        if a_ub < b_lb || b_ub < a_lb {
            return Ok(());
        }

        if a_lb == a_ub {
            context.post(
                predicate!(self.b != a_lb),
                (conjunction!([self.a == a_lb]), &self.inference_code),
            )?;
        }

        if b_lb == b_ub {
            context.post(
                predicate!(self.a != b_lb),
                (conjunction!([self.b == b_lb]), &self.inference_code),
            )?;
        }

        Ok(())
    }
}
