use std::cell::OnceCell;
use std::cmp::max;
use std::cmp::min;
use std::rc::Rc;

use crate::basic_types::PropagationStatusCP;
use crate::conjunction;
use crate::engine::cp::propagation::ReadDomains;
use crate::engine::domain_events::DomainEvents;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::PropagatorVariable;
use crate::engine::variables::IntegerVariable;
use crate::predicate;

pub(crate) struct ElementConstructor<VX, VI, VE> {
    pub(crate) array: Box<[VX]>,
    pub(crate) index: VI,
    pub(crate) rhs: VE,
}

/// Arc-consistent propagator for constraint `element([x_1, \ldots, x_n], i, e)`, where `x_j` are
///  variables, `i` is an integer variable, and `e` is a variable, which holds iff `x_i = e`
///
/// Note that this propagator is 0-indexed
pub(crate) struct ElementPropagator<VX, VI, VE> {
    array: Rc<[PropagatorVariable<VX>]>,
    index: PropagatorVariable<VI>,
    rhs: PropagatorVariable<VE>,
}

const ID_INDEX: LocalId = LocalId::from(0);
const ID_RHS: LocalId = LocalId::from(1);
// local ids of array vars are shifted by ID_X_OFFSET
const ID_X_OFFSET: u32 = 2;

/// Iterator through the domain values of an IntegerVariable; keeps a reference to the context
/// Use `for_domain_values!` if you want mutable access to the context while iterating
fn iter_values<'c, Var: IntegerVariable>(
    context: &'c PropagationContextMut,
    var: &'c PropagatorVariable<Var>,
) -> impl Iterator<Item = i32> + 'c {
    (context.lower_bound(var)..=context.upper_bound(var)).filter(|i| context.contains(var, *i))
}

/// Helper to loop through the domain values of an IntegerVariable without keeping a reference to
/// the context
macro_rules! for_domain_values {
    ($context:expr, $var:expr, |$val:ident| $body:expr) => {
        for $val in ($context.lower_bound($var)..=$context.upper_bound($var)) {
            if $context.contains($var, $val) {
                $body
            }
        }
    };
}

impl<VX: IntegerVariable + 'static, VI: IntegerVariable, VE: IntegerVariable> PropagatorConstructor
    for ElementConstructor<VX, VI, VE>
{
    type Propagator = ElementPropagator<VX, VI, VE>;

    fn create(self, mut context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        // local ids of array vars are shifted by ID_X_OFFSET
        let array = self
            .array
            .iter()
            .enumerate()
            .map(|(i, x_i)| {
                context.register(
                    x_i.clone(),
                    DomainEvents::ANY_INT,
                    LocalId::from(i as u32 + ID_X_OFFSET),
                )
            })
            .collect();
        ElementPropagator {
            array,
            index: context.register(self.index, DomainEvents::ANY_INT, ID_INDEX),
            rhs: context.register(self.rhs, DomainEvents::ANY_INT, ID_RHS),
        }
    }
}

impl<VX: IntegerVariable + 'static, VI: IntegerVariable, VE: IntegerVariable> Propagator
    for ElementPropagator<VX, VI, VE>
{
    fn propagate(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        // For incremental solving: use the doubly linked list data-structure
        if context.is_fixed(&self.index) {
            // At this point, we should post x_i = e as a new constraint, but that's not an option
            //  in Pumpkin right now. So instead we manually make them equal
            let i = context.lower_bound(&self.index);
            let x_i = &self.array[i as usize];

            let lb = max(context.lower_bound(&self.rhs), context.lower_bound(x_i));
            let ub = min(context.upper_bound(&self.rhs), context.upper_bound(x_i));

            context.set_lower_bound(
                &self.rhs,
                lb,
                conjunction!([self.index == i] & [x_i >= lb]),
            )?;
            context.set_lower_bound(x_i, lb, conjunction!([self.index == i] & [self.rhs >= lb]))?;
            context.set_upper_bound(
                &self.rhs,
                ub,
                conjunction!([self.index == i] & [x_i <= ub]),
            )?;
            context.set_upper_bound(x_i, ub, conjunction!([self.index == i] & [self.rhs <= ub]))?;

            for v in lb..=ub {
                if !context.contains(&self.rhs, v) && context.contains(x_i, v) {
                    context.remove(x_i, v, conjunction!([self.index == i] & [self.rhs != v]))?;
                } else if context.contains(&self.rhs, v) && !context.contains(x_i, v) {
                    context.remove(
                        &self.rhs,
                        v,
                        conjunction!([self.index == i] & [self.array[i as usize] != v]),
                    )?;
                }
            }
        } else {
            // Remove values from i when for no values of e: x_i = e
            let index_reason = OnceCell::new();
            for_domain_values!(context, &self.index, |i| {
                let x_i = &self.array[i as usize];
                if !iter_values(context, &self.rhs).any(|e| context.contains(x_i, e)) {
                    // N.B. index_reason is loop-independent
                    let reason_info = Rc::clone(index_reason.get_or_init(|| {
                        Rc::new((
                            context.describe_domain(&self.rhs),
                            iter_values(context, &self.rhs).collect::<Vec<_>>(),
                        ))
                    }));
                    let x_i = (*x_i).clone();
                    context.remove(&self.index, i, move |_context: &PropagationContext| {
                        let mut reason = reason_info.0.clone();
                        reason_info
                            .1
                            .iter()
                            .for_each(|e| reason.push(predicate![x_i != *e]));
                        reason.into()
                    })?;
                }
            });

            // Remove values from e when for no values of i: x_i = e
            let rhs_reason = OnceCell::new();
            for_domain_values!(context, &self.rhs, |e| {
                if !iter_values(context, &self.index)
                    .map(|i| &self.array[i as usize])
                    .any(|x_i| context.contains(x_i, e))
                {
                    // N.B. rhs_reason is loop-independent
                    let reason_info = Rc::clone(rhs_reason.get_or_init(|| {
                        Rc::new((
                            context.describe_domain(&self.index),
                            iter_values(context, &self.index).collect::<Vec<_>>(),
                        ))
                    }));
                    let array = Rc::clone(&self.array);
                    context.remove(&self.rhs, e, move |_context: &PropagationContext| {
                        let mut reason = reason_info.0.clone();
                        reason_info
                            .1
                            .iter()
                            .for_each(|i| reason.push(predicate![array[*i as usize] != e]));
                        reason.into()
                    })?;
                }
            });
        }
        Ok(())
    }

    fn synchronise(&mut self, _context: &PropagationContext) {}

    fn priority(&self) -> u32 {
        // Priority higher than int_times/linear_eq/not_eq_propagator because it's much more
        //  expensive looping over multiple domains
        2
    }

    fn name(&self) -> &str {
        "Element"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContextMut) -> PropagationStatusCP {
        // Ensure index is non-negative
        context.set_lower_bound(&self.index, 0, conjunction!())?;
        // Ensure index <= no. of x_j
        context.set_upper_bound(&self.index, self.array.len() as i32, conjunction!())?;

        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> PropagationStatusCP {
        // Close to duplicate of `propagate` for now, without saving reason stuff...
        if context.is_fixed(&self.index) {
            let i = context.lower_bound(&self.index);
            let x_i = &self.array[i as usize];

            let lb = min(context.lower_bound(&self.rhs), context.lower_bound(x_i));
            let ub = max(context.upper_bound(&self.rhs), context.upper_bound(x_i));

            context.set_lower_bound(&self.rhs, lb, conjunction!())?;
            context.set_lower_bound(x_i, lb, conjunction!())?;
            context.set_upper_bound(&self.rhs, ub, conjunction!())?;
            context.set_upper_bound(x_i, ub, conjunction!())?;

            for_domain_values!(context, x_i, |e| {
                if !context.contains(&self.rhs, e) {
                    context.remove(x_i, e, conjunction!())?;
                }
            });
            for_domain_values!(context, &self.rhs, |e| {
                if !context.contains(x_i, e) {
                    context.remove(&self.rhs, e, conjunction!())?;
                }
            });
        } else {
            // Remove values from i when for no values of e: x_i = e
            for_domain_values!(context, &self.index, |i| {
                let x_i = &self.array[i as usize];
                if !iter_values(context, &self.rhs).any(|e| context.contains(x_i, e)) {
                    context.remove(&self.index, i, conjunction!())?;
                }
            });
            // Remove values from e when for no values of i: x_i = e
            for_domain_values!(context, &self.rhs, |e| {
                if !iter_values(context, &self.index)
                    .map(|i| &self.array[i as usize])
                    .any(|x_i| context.contains(x_i, e))
                {
                    context.remove(&self.rhs, e, conjunction!())?;
                }
            });
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;

    #[test]
    fn cocp_m4co_example() {
        // source: https://user.it.uu.se/~pierref/courses/COCP/slides/T16-Propagators.pdf#Navigation24
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(4, 4);
        let x_1 = solver.new_variable(5, 5);
        let x_2 = solver.new_variable(9, 9);
        let x_3 = solver.new_variable(7, 7);
        let index = solver.new_variable(1, 3);
        let rhs = solver.new_variable(2, 8);
        let array = vec![x_0, x_1, x_2, x_3].into_boxed_slice();

        let mut propagator = solver
            .new_propagator(ElementConstructor { array, index, rhs })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        assert_eq!(1, solver.lower_bound(index));
        assert_eq!(3, solver.upper_bound(index));
        assert!(!solver.contains(index, 2));
        assert_eq!(5, solver.lower_bound(rhs));
        assert_eq!(7, solver.upper_bound(rhs));
        assert!(!solver.contains(rhs, 6));
    }

    #[test]
    fn reason_test() {
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(4, 6);
        let x_1 = solver.new_variable(7, 8);
        let x_2 = solver.new_variable(7, 9);
        let x_3 = solver.new_variable(10, 11);
        let index = solver.new_variable(1, 3);
        let rhs = solver.new_variable(6, 9);
        let array = vec![x_0, x_1, x_2, x_3].into_boxed_slice();

        let mut propagator = solver
            .new_propagator(ElementConstructor { array, index, rhs })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        let index_reason = solver.get_reason_int(predicate![index != 3]);
        // reason for index removal 3 is that `x_3 != e`
        assert_eq!(
            *index_reason,
            conjunction!(
                [rhs >= 6] & [rhs <= 9] & [x_3 != 6] & [x_3 != 7] & [x_3 != 8] & [x_3 != 9]
            )
        );

        let rhs_reason = solver.get_reason_int(predicate![rhs != 6]);
        // reason for rhs removal 6 is that for all valid indices i `x_i != 6`
        assert_eq!(
            *rhs_reason,
            conjunction!([index >= 1] & [index <= 2] & [x_1 != 6] & [x_2 != 6])
        );
    }

    #[test]
    fn reason_test_fixed_index() {
        let mut solver = TestSolver::default();
        let x_0 = solver.new_variable(4, 6);
        let x_1 = solver.new_variable(7, 10);
        let x_2 = solver.new_variable(7, 9);
        let x_3 = solver.new_variable(10, 11);
        let index = solver.new_variable(1, 1);
        let rhs = solver.new_variable(6, 9);
        solver.remove(rhs, 8).expect("no empty domains");

        let array = vec![x_0, x_1, x_2, x_3].into_boxed_slice();

        let mut propagator = solver
            .new_propagator(ElementConstructor { array, index, rhs })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        let x1_ub_reason = solver.get_reason_int(predicate![x_1 <= 9]);
        // reason for x_1 <= 9 is that `x_1 == e` and `e <= 9`
        assert_eq!(*x1_ub_reason, conjunction!([index == 1] & [rhs <= 9]));

        let x1_8_reason = solver.get_reason_int(predicate![x_1 != 8]);
        // reason for x_1 removal 8 is that `x_1 == e` and `e != 8`
        assert_eq!(*x1_8_reason, conjunction!([index == 1] & [rhs != 8]));

        let rhs_reason = solver.get_reason_int(predicate![rhs >= 7]);
        // reason for `rhs >= 7` is that `x_1 >= 7`
        assert_eq!(*rhs_reason, conjunction!([index == 1] & [x_1 >= 7]));
    }
}
