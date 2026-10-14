//! Propagator for `lex_less_or_equal(x, y)`: the vector `x` is lexicographically at most `y`.
//!
//! Let `alpha` be the first position at which `x` and `y` are not both fixed to the same value.
//! Every earlier position is equal, so the constraint requires `x[alpha] <= y[alpha]`, and
//! `x[alpha] < y[alpha]` if the positions after `alpha` cannot be lexicographically ordered: that
//! is the case exactly when the lower bounds of `x` after `alpha` are lexicographically greater
//! than the upper bounds of `y` after `alpha`. The propagator enforces these bounds on
//! `x[alpha]` and `y[alpha]` and repeats while `alpha` moves forward.
//!
//! For example, with `x = [2, x1]`, `y = [2, y1]`, `x1` in `3..5` and `y1` in `0..4`, the first
//! position is fixed and equal, so `alpha = 1`: `x1 <= 4` and `y1 >= 3`. With `x = [x0, 4]`,
//! `y = [y0, 3]` and `x0, y0` in `1..3`, the last position rules out `x0 = y0`, so `x0 <= 2` and
//! `y0 >= 2`.

use std::borrow::Cow;
use std::marker::PhantomData;

use pumpkin_checking::AtomicConstraint;
use pumpkin_checking::CheckerVariable;
use pumpkin_checking::ConflictCheck;
use pumpkin_checking::ConflictChecker;
use pumpkin_checking::DomainView;
use pumpkin_checking::RetentionCheck;
use pumpkin_checking::RetentionChecker;
use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::ConstraintTag;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::ConflictRule;
use pumpkin_core::propagation::ConstraintDescription;
use pumpkin_core::propagation::DomainEvents;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::EventsToRegister;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::PropagatorConstructor;
use pumpkin_core::propagation::PropagatorConstructorContext;
use pumpkin_core::propagation::PropagatorWithEvents;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::SolutionCheck;
use pumpkin_core::propagation::TrailedInteger;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

pumpkin_core::scoped_struct! {
/// The description of the constraint that `x` is lexicographically at most `y`.
#[derive(Clone, Debug)]
pub struct LexLessOrEqualDescription<Var> {
    pub x: Box<[Var]>,
    pub y: Box<[Var]>,
}
}

impl<Var: IntegerVariable> ConstraintDescription for LexLessOrEqualDescription<Var> {
    fn check_solution(&self, domains: &dyn DomainView<Predicate>) -> SolutionCheck {
        for (x, y) in self.x.iter().zip(self.y.iter()) {
            let (Some(x), Some(y)) = (
                x.induced_fixed_value(domains),
                y.induced_fixed_value(domains),
            ) else {
                return SolutionCheck::Unknown;
            };
            if x < y {
                return SolutionCheck::ConstraintSatisfied;
            }
            if x > y {
                return SolutionCheck::ConstraintViolated;
            }
        }
        SolutionCheck::ConstraintSatisfied
    }
}

/// The rule of [`LexLessOrEqualPropagator`].
#[derive(Clone, Copy, Debug)]
pub struct LexLessOrEqualRule<Var>(PhantomData<Var>);

impl<Var: IntegerVariable + 'static> ConflictRule for LexLessOrEqualRule<Var> {
    type Description = LexLessOrEqualDescription<Var>;

    fn name() -> Cow<'static, str> {
        Cow::Borrowed("lex_less_or_equal")
    }

    fn create_conflict_checker(
        constraint_description: &LexLessOrEqualDescription<Var>,
    ) -> impl ConflictChecker<Predicate> + 'static {
        LexLessOrEqualChecker {
            x: constraint_description.x.clone(),
            y: constraint_description.y.clone(),
        }
    }

    fn create_retention_checker(
        _: &LexLessOrEqualDescription<Var>,
    ) -> impl RetentionChecker<Predicate> + 'static {
        NoRetentionCheck
    }
}

/// Accepts every state; a stand-in until the rule has a retention checker of its own.
#[derive(Clone, Copy, Debug)]
struct NoRetentionCheck;

impl<Atomic: AtomicConstraint> RetentionChecker<Atomic> for NoRetentionCheck {
    fn check_retention(&self, _: &dyn DomainView<Atomic>) -> RetentionCheck {
        RetentionCheck::NothingToPropagate
    }
}

#[derive(Clone, Debug)]
pub struct LexLessOrEqualArgs<Var> {
    pub constraint_description: LexLessOrEqualDescription<Var>,
    pub constraint_tag: ConstraintTag,
}

impl<Var> PropagatorConstructor for LexLessOrEqualArgs<Var>
where
    Var: IntegerVariable + 'static,
{
    type PropagatorImpl = LexLessOrEqualPropagator<Var>;
    type Rule = LexLessOrEqualRule<Var>;

    fn constraint_description(&self) -> LexLessOrEqualDescription<Var> {
        self.constraint_description.clone()
    }

    fn constraint_tag(&self) -> ConstraintTag {
        self.constraint_tag
    }

    fn create(
        self,
        mut context: PropagatorConstructorContext,
        inference_code: InferenceCode,
    ) -> PropagatorWithEvents<Self::PropagatorImpl> {
        let LexLessOrEqualArgs {
            constraint_description: LexLessOrEqualDescription { x, y },
            constraint_tag: _,
        } = self;
        assert_eq!(
            x.len(),
            y.len(),
            "lex_less_or_equal needs vectors of equal length"
        );

        let mut registration = EventsToRegister::builder();
        for (index, var) in x.iter().chain(y.iter()).enumerate() {
            registration = registration.add(var, DomainEvents::BOUNDS, LocalId::from(index as u32));
        }

        PropagatorWithEvents {
            events_to_register: registration.build(),
            propagator: LexLessOrEqualPropagator {
                alpha: context.new_trailed_integer(0),
                beta: context.new_trailed_integer(x.len() as i64),
                x,
                y,
                inference_code,
                reason_buffer: Vec::new(),
            },
        }
    }
}

/// Bounds propagator for `x <=lex y`. Can be constructed through [`LexLessOrEqualArgs`].
///
/// `alpha`, the first position where `x` and `y` are not both fixed to the same value, only
/// moves forward along a branch, so it is kept as backtrackable state and each call continues
/// from it. `beta` is the first position after `alpha` whose bounds decide whether
/// `x[alpha] = y[alpha]` is possible; changes after `beta` cannot affect the propagation and are
/// ignored. Explanations are built only when conflict analysis asks for them.
#[derive(Clone, Debug)]
pub struct LexLessOrEqualPropagator<Var> {
    x: Box<[Var]>,
    y: Box<[Var]>,
    inference_code: InferenceCode,
    alpha: TrailedInteger,
    beta: TrailedInteger,
    reason_buffer: Vec<Predicate>,
}

/// What a lazily explained propagation bounded: the upper bound of `x[alpha]` or the lower bound
/// of `y[alpha]`, and whether strictly.
fn encode(alpha: usize, bounds_y: bool, strict: bool) -> usize {
    (alpha << 2) | (usize::from(bounds_y) << 1) | usize::from(strict)
}

fn decode(code: u64) -> (usize, bool, bool) {
    let code = code as usize;
    (code >> 2, code & 2 != 0, code & 1 != 0)
}

impl<Var: IntegerVariable + 'static> Propagator for LexLessOrEqualPropagator<Var> {
    fn priority(&self) -> Priority {
        Priority::High
    }

    fn name(&self) -> &str {
        "LexLessOrEqual"
    }

    fn notify(
        &mut self,
        context: NotificationContext,
        local_id: LocalId,
        _event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        let position = local_id.unpack() as usize % self.x.len();
        if position as i64 > context.read_trailed_integer(self.beta) {
            EnqueueDecision::Skip
        } else {
            EnqueueDecision::Enqueue
        }
    }

    fn propagate(&mut self, mut context: PropagationContext) -> PropagationStatusCP {
        let n = self.x.len();
        let mut alpha = context.read_trailed_integer(self.alpha) as usize;
        loop {
            while alpha < n && fixed_and_equal(&context, &self.x[alpha], &self.y[alpha]) {
                alpha += 1;
            }
            context.write_trailed_integer(self.alpha, alpha as i64);
            if alpha == n {
                context.write_trailed_integer(self.beta, n as i64);
                return Ok(());
            }

            // The first position after alpha where the bounds decide.
            let mut beta = alpha + 1;
            let mut strict = false;
            while beta < n {
                let lower = context.lower_bound(&self.x[beta]);
                let upper = context.upper_bound(&self.y[beta]);
                if lower != upper {
                    strict = lower > upper;
                    break;
                }
                beta += 1;
            }
            context.write_trailed_integer(self.beta, beta as i64);

            let gap = i32::from(strict);
            let (x, y) = (&self.x[alpha], &self.y[alpha]);
            let y_upper = context.upper_bound(y);
            if context.upper_bound(x) > y_upper - gap {
                context.post(predicate![x <= y_upper - gap], encode(alpha, false, strict))?;
            }
            let x_lower = context.lower_bound(x);
            if context.lower_bound(y) < x_lower + gap {
                context.post(predicate![y >= x_lower + gap], encode(alpha, true, strict))?;
            }

            if !fixed_and_equal(&context, x, y) {
                return Ok(());
            }
        }
    }

    fn lazy_explanation(&mut self, code: u64, context: ExplanationContext) -> LazyExplanation<'_> {
        let (alpha, bounds_y, strict) = decode(code);
        let position = context.get_trail_position();
        self.reason_buffer.clear();
        for j in 0..alpha {
            let value = context.lower_bound_at_trail_position(&self.x[j], position);
            self.reason_buffer.push(predicate![self.x[j] == value]);
            self.reason_buffer.push(predicate![self.y[j] == value]);
        }
        if bounds_y {
            let lower = context.lower_bound_at_trail_position(&self.x[alpha], position);
            self.reason_buffer.push(predicate![self.x[alpha] >= lower]);
        } else {
            let upper = context.upper_bound_at_trail_position(&self.y[alpha], position);
            self.reason_buffer.push(predicate![self.y[alpha] <= upper]);
        }
        if strict {
            for k in alpha + 1..self.x.len() {
                let lower = context.lower_bound_at_trail_position(&self.x[k], position);
                let upper = context.upper_bound_at_trail_position(&self.y[k], position);
                self.reason_buffer.push(predicate![self.x[k] >= lower]);
                self.reason_buffer.push(predicate![self.y[k] <= upper]);
                if lower != upper {
                    break;
                }
            }
        }
        LazyExplanation {
            predicates: self.reason_buffer.as_slice(),
            inference_code: self.inference_code,
        }
    }
}

fn fixed_and_equal<Var: IntegerVariable>(context: &PropagationContext, x: &Var, y: &Var) -> bool {
    let value = context.lower_bound(x);
    context.upper_bound(x) == value
        && context.lower_bound(y) == value
        && context.upper_bound(y) == value
}

/// Checks that a state is infeasible for `x <=lex y`: within the bounds, the smallest possible
/// `x` is lexicographically greater than the largest possible `y`.
#[derive(Clone, Debug)]
pub struct LexLessOrEqualChecker<Var> {
    pub x: Box<[Var]>,
    pub y: Box<[Var]>,
}

impl<Var, Atomic> ConflictChecker<Atomic> for LexLessOrEqualChecker<Var>
where
    Atomic: AtomicConstraint,
    Var: CheckerVariable<Atomic>,
{
    fn check(
        &self,
        state: &pumpkin_checking::VariableState<Atomic>,
        _: &[Atomic],
        _: Option<&Atomic>,
    ) -> ConflictCheck {
        for (x, y) in self.x.iter().zip(self.y.iter()) {
            let lower = x.induced_lower_bound(state);
            let upper = y.induced_upper_bound(state);
            if lower < upper {
                return ConflictCheck::NoConflictDetected;
            }
            if lower > upper {
                return ConflictCheck::ConflictDetected;
            }
        }
        ConflictCheck::NoConflictDetected
    }
}

#[cfg(test)]
mod tests {
    use pumpkin_core::state::State;

    use super::*;
    use crate::StateExt;

    fn post(
        state: &mut State,
        x: Vec<pumpkin_core::variables::DomainId>,
        y: Vec<pumpkin_core::variables::DomainId>,
    ) {
        let constraint_tag = state.new_constraint_tag();
        let _ = state.add_propagator(LexLessOrEqualArgs {
            constraint_description: LexLessOrEqualDescription {
                x: x.into(),
                y: y.into(),
            },
            constraint_tag,
        });
    }

    #[test]
    fn the_first_position_is_bounded_by_the_other_side() {
        let mut state = State::default();
        let x0 = state.new_interval_variable(2, 5, None);
        let x1 = state.new_interval_variable(0, 9, None);
        let y0 = state.new_interval_variable(0, 3, None);
        let y1 = state.new_interval_variable(0, 9, None);
        post(&mut state, vec![x0, x1], vec![y0, y1]);
        state.propagate_to_fixed_point().expect("no empty domain");

        state.assert_bounds(x0, 2, 3);
        state.assert_bounds(y0, 2, 3);
        state.assert_bounds(x1, 0, 9);
    }

    #[test]
    fn an_infeasible_tail_makes_the_first_position_strict() {
        let mut state = State::default();
        let x0 = state.new_interval_variable(1, 3, None);
        let x1 = state.new_interval_variable(4, 5, None);
        let y0 = state.new_interval_variable(1, 3, None);
        let y1 = state.new_interval_variable(0, 3, None);
        post(&mut state, vec![x0, x1], vec![y0, y1]);
        state.propagate_to_fixed_point().expect("no empty domain");

        state.assert_bounds(x0, 1, 2);
        state.assert_bounds(y0, 2, 3);
    }

    #[test]
    fn a_fixed_equal_prefix_moves_the_propagation_on() {
        let mut state = State::default();
        let x0 = state.new_interval_variable(2, 2, None);
        let y0 = state.new_interval_variable(2, 2, None);
        let x1 = state.new_interval_variable(3, 5, None);
        let y1 = state.new_interval_variable(0, 4, None);
        post(&mut state, vec![x0, x1], vec![y0, y1]);
        state.propagate_to_fixed_point().expect("no empty domain");

        state.assert_bounds(x1, 3, 4);
        state.assert_bounds(y1, 3, 4);
    }

    #[test]
    fn forcing_the_first_position_equal_continues_with_the_next() {
        // x0 in 3..4 and y0 = 3 force x0 = 3; then x1 <= y1 applies.
        let mut state = State::default();
        let x0 = state.new_interval_variable(3, 4, None);
        let y0 = state.new_interval_variable(3, 3, None);
        let x1 = state.new_interval_variable(5, 9, None);
        let y1 = state.new_interval_variable(0, 6, None);
        post(&mut state, vec![x0, x1], vec![y0, y1]);
        state.propagate_to_fixed_point().expect("no empty domain");

        state.assert_bounds(x0, 3, 3);
        state.assert_bounds(x1, 5, 6);
        state.assert_bounds(y1, 5, 6);
    }

    #[test]
    fn a_greater_fixed_vector_is_a_conflict() {
        let mut state = State::default();
        let x0 = state.new_interval_variable(3, 3, None);
        let y0 = state.new_interval_variable(2, 2, None);
        post(&mut state, vec![x0], vec![y0]);
        assert!(state.propagate_to_fixed_point().is_err());
    }
}
