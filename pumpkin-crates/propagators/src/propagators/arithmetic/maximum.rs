use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::I32Ext;
use pumpkin_checking::InferenceChecker;
use pumpkin_core::conjunction;
use pumpkin_core::declare_inference_label;
use pumpkin_core::predicate;
use pumpkin_core::predicates::PropositionalConjunction;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::InferenceCheckers;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::results::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

#[derive(Clone, Debug)]
pub struct MaximumArgs<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
    pub constraint_tag: ConstraintTag,
}

declare_inference_label!(Maximum);

impl<ElementVar, Rhs> PropagatorConstructor for MaximumArgs<ElementVar, Rhs>
where
    ElementVar: IntegerVariable + 'static,
    Rhs: IntegerVariable + 'static,
{
    type PropagatorImpl = MaximumPropagator<ElementVar, Rhs>;

    fn add_inference_checkers(&self, mut checkers: InferenceCheckers<'_>) {
        checkers.add_inference_checker(
            InferenceCode::new(self.constraint_tag, Maximum),
            Box::new(MaximumChecker {
                array: self.array.clone(),
                rhs: self.rhs.clone(),
            }),
        );
    }

    fn create(self, mut context: PropagatorConstructorContext) -> Self::PropagatorImpl {
        let MaximumArgs {
            array,
            rhs,
            constraint_tag,
        } = self;

        for (idx, var) in array.iter().enumerate() {
            context.register(var.clone(), DomainEvents::BOUNDS, LocalId::from(idx as u32));
        }

        context.register(
            rhs.clone(),
            DomainEvents::BOUNDS,
            LocalId::from(array.len() as u32),
        );

        let inference_code = InferenceCode::new(constraint_tag, Maximum);

        MaximumPropagator {
            array,
            rhs,
            inference_code,
        }
    }
}

/// Bounds-consistent propagator which enforces `max(array) = rhs`. Can be constructed through
/// [`MaximumArgs`].
#[derive(Clone, Debug)]
pub struct MaximumPropagator<ElementVar, Rhs> {
    array: Box<[ElementVar]>,
    rhs: Rhs,
    inference_code: InferenceCode,
}

impl<ElementVar: IntegerVariable + 'static, Rhs: IntegerVariable + 'static> Propagator
    for MaximumPropagator<ElementVar, Rhs>
{
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "Maximum"
    }

    fn propagate_from_scratch(&self, mut context: PropagationContext) -> PropagationStatusCP {
        // This is the constraint that is being propagated:
        // max(a_0, a_1, ..., a_{n-1}) = rhs

        let rhs_ub = context.upper_bound(&self.rhs);
        let mut max_ub = context.upper_bound(&self.array[0]);
        let mut max_lb = context.lower_bound(&self.array[0]);
        let mut lb_reason = predicate![self.array[0] >= max_lb];
        for var in self.array.iter() {
            // Rule 1.
            // UB(a_i) <= UB(rhs, constraint_tag }
            context.post(
                predicate![var <= rhs_ub],
                conjunction!([self.rhs <= rhs_ub]),
                &self.inference_code,
            )?;

            let var_lb = context.lower_bound(var);
            let var_ub = context.upper_bound(var);

            if var_lb > max_lb {
                max_lb = var_lb;
                lb_reason = predicate![var >= var_lb];
            }

            if var_ub > max_ub {
                max_ub = var_ub;
            }
        }
        // Rule 2.
        // LB(rhs, constraint_tag } >= max{LB(a_i)}.
        context.post(
            predicate![self.rhs >= max_lb],
            PropositionalConjunction::from(lb_reason),
            &self.inference_code,
        )?;

        // Rule 3.
        // UB(rhs, constraint_tag } <= max{UB(a_i)}.
        // Note that this implicitly also covers the rule:
        // 'if LB(rhs, constraint_tag } > UB(a_i) for all i, then conflict'.
        if rhs_ub > max_ub {
            let ub_reason: PropositionalConjunction = self
                .array
                .iter()
                .map(|var| predicate![var <= max_ub])
                .collect();
            context.post(
                predicate![self.rhs <= max_ub],
                ub_reason,
                &self.inference_code,
            )?;
        }

        // Rule 4.
        // If there is only one variable with UB(a_i) >= LB(rhs, constraint_tag },
        // then the bounds for rhs and that variable should be intersected.
        let rhs_lb = context.lower_bound(&self.rhs);
        let mut propagating_variable: Option<&ElementVar> = None;
        let mut propagation_reason = PropositionalConjunction::default();
        for var in self.array.iter() {
            if context.upper_bound(var) >= rhs_lb {
                if propagating_variable.is_none() {
                    propagating_variable = Some(var);
                } else {
                    propagating_variable = None;
                    break;
                }
            } else {
                propagation_reason.push(predicate![var <= rhs_lb - 1]);
            }
        }
        // If there is exactly one variable UB(a_i) >= LB(rhs, constraint_tag }, then the
        // propagating variable is Some. In that case, intersect the bounds of that variable
        // and the rhs. Given previous rules, only the lower bound of the propagated
        // variable needs to be propagated.
        if let Some(propagating_variable) = propagating_variable {
            let var_lb = context.lower_bound(propagating_variable);
            if var_lb < rhs_lb {
                propagation_reason.push(predicate![self.rhs >= rhs_lb]);
                context.post(
                    predicate![propagating_variable >= rhs_lb],
                    propagation_reason,
                    &self.inference_code,
                )?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct MaximumChecker<ElementVar, Rhs> {
    pub array: Box<[ElementVar]>,
    pub rhs: Rhs,
}

impl<ElementVar, Rhs, Atomic> InferenceChecker<Atomic> for MaximumChecker<ElementVar, Rhs>
where
    Atomic: AtomicConstraint,
    ElementVar: CheckerVariable<Atomic>,
    Rhs: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> bool {
        let lowest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_lower_bound(&state))
            .max()
            .unwrap_or(I32Ext::NegativeInf);
        let highest_maximum = self
            .array
            .iter()
            .map(|element| element.induced_upper_bound(&state))
            .max()
            .unwrap_or(I32Ext::PositiveInf);

        // If the intersection between the domain of `rhs` and `[lowest_maximum,
        // highest_maximum]` is empty, there is a conflict.

        lowest_maximum > self.rhs.induced_upper_bound(&state)
            || highest_maximum < self.rhs.induced_lower_bound(&state)
    }
}

#[allow(deprecated, reason = "Will be refactored")]
#[cfg(test)]
mod tests {
    use pumpkin_core::TestSolver;

    use super::*;

    #[test]
    fn upper_bound_of_rhs_matches_maximum_upper_bound_of_array_at_initialise() {
        let mut solver = TestSolver::default();

        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(1, 4);
        let c = solver.new_variable(1, 5);

        let rhs = solver.new_variable(1, 10);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(MaximumArgs {
                array: [a, b, c].into(),
                rhs,
                constraint_tag,
            })
            .expect("no empty domain");

        solver.assert_bounds(rhs, 1, 5);

        let reason = solver.get_reason_int(predicate![rhs <= 5]);
        assert_eq!(conjunction!([a <= 5] & [b <= 5] & [c <= 5]), reason);
    }

    #[test]
    fn lower_bound_of_rhs_is_maximum_of_lower_bounds_in_array() {
        let mut solver = TestSolver::default();

        let a = solver.new_variable(3, 10);
        let b = solver.new_variable(4, 10);
        let c = solver.new_variable(5, 10);

        let rhs = solver.new_variable(1, 10);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(MaximumArgs {
                array: [a, b, c].into(),
                rhs,
                constraint_tag,
            })
            .expect("no empty domain");

        solver.assert_bounds(rhs, 5, 10);

        let reason = solver.get_reason_int(predicate![rhs >= 5]);
        assert_eq!(conjunction!([c >= 5]), reason);
    }

    #[test]
    fn upper_bound_of_all_array_elements_at_most_rhs_max_at_initialise() {
        let mut solver = TestSolver::default();

        let array = (1..=5)
            .map(|idx| solver.new_variable(1, 4 + idx))
            .collect::<Box<_>>();

        let rhs = solver.new_variable(1, 3);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(MaximumArgs {
                array: array.clone(),
                rhs,
                constraint_tag,
            })
            .expect("no empty domain");

        for var in array.iter() {
            solver.assert_bounds(*var, 1, 3);
            let reason = solver.get_reason_int(predicate![var <= 3]);
            assert_eq!(conjunction!([rhs <= 3]), reason);
        }
    }

    #[test]
    fn single_variable_propagate() {
        let mut solver = TestSolver::default();

        let array = (1..=5)
            .map(|idx| solver.new_variable(1, 1 + 10 * idx))
            .collect::<Box<_>>();

        let rhs = solver.new_variable(45, 60);
        let constraint_tag = solver.new_constraint_tag();

        let _ = solver
            .new_propagator(MaximumArgs {
                array: array.clone(),
                rhs,
                constraint_tag,
            })
            .expect("no empty domain");

        solver.assert_bounds(*array.last().unwrap(), 45, 51);
        solver.assert_bounds(rhs, 45, 51);
    }
}
