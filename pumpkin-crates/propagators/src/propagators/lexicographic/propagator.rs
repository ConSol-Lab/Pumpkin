use pumpkin_core::predicate;
use pumpkin_core::predicates::Predicate;
use pumpkin_core::proof::InferenceCode;
use pumpkin_core::propagation::EnqueueDecision;
use pumpkin_core::propagation::ExplanationContext;
use pumpkin_core::propagation::LazyExplanation;
use pumpkin_core::propagation::LocalId;
use pumpkin_core::propagation::NotificationContext;
use pumpkin_core::propagation::OpaqueDomainEvent;
use pumpkin_core::propagation::Priority;
use pumpkin_core::propagation::PropagationContext;
use pumpkin_core::propagation::Propagator;
use pumpkin_core::propagation::ReadDomains;
use pumpkin_core::propagation::TrailedInteger;
use pumpkin_core::state::PropagationStatusCP;
use pumpkin_core::variables::IntegerVariable;

#[cfg(doc)]
use super::LexLessOrEqualArgs;

/// Bounds propagator for `x <=lex y`. Can be constructed through [`LexLessOrEqualArgs`].
///
/// `alpha`, the first position where `x` and `y` are not both fixed to the same value, only
/// moves forward along a branch, so it is kept as backtrackable state and each call continues
/// from it. `beta` is the first position after `alpha` whose bounds decide whether
/// `x[alpha] = y[alpha]` is possible; changes after `beta` cannot affect the propagation and are
/// ignored. Explanations are built only when conflict analysis asks for them.
#[derive(Clone, Debug)]
pub struct LexLessOrEqualPropagator<Var> {
    pub(crate) x: Box<[Var]>,
    pub(crate) y: Box<[Var]>,
    pub(crate) inference_code: InferenceCode,
    pub(crate) alpha: TrailedInteger,
    pub(crate) beta: TrailedInteger,
    pub(crate) reason_buffer: Vec<Predicate>,
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
