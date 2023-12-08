use slice_dst::SliceWithHeader;
use std::cell::OnceCell;
use std::cmp::{max, min};
use std::collections::HashMap;
use std::rc::Rc;

use crate::basic_types::variables::IntVar;
use crate::basic_types::{PropagationStatusCP, PropositionalConjunction};
use crate::engine::{
    CPPropagatorConstructor, ConstraintProgrammingPropagator, Delta, DomainChange, DomainEvents,
    LocalId, PropagationContext, PropagatorConstructorContext, PropagatorVariable,
};
use crate::{conjunction, predicate};

pub struct ElementArgs<VX, VI, VE> {
    pub array: Box<[VX]>,
    pub index: VI,
    pub rhs: VE,
}

/// Arc-consistent propagator for constraint `element([x_1, \ldots, x_n], i, e)`, where `x_j` are
///  variables, `i` is an integer variable, and `e` is a variable, which holds iff `x_i = e`
pub struct Element<VX, VI, VE> {
    array: Box<[PropagatorVariable<VX>]>,
    index: PropagatorVariable<VI>,
    rhs: PropagatorVariable<VE>,
    propagations_index: HashMap<i32, Rc<SliceWithHeader<PropositionalConjunction, i32>>>,
    propagations_rhs: HashMap<i32, Rc<SliceWithHeader<PropositionalConjunction, i32>>>,
}

const ID_INDEX: LocalId = LocalId::from(0);
const ID_RHS: LocalId = LocalId::from(1);
// local ids of array vars are shifted by ID_X_OFFSET
const ID_X_OFFSET: u32 = 2;

/// Iterator through the domain values of an IntVar; keeps a reference to the context
/// Use `for_domain_values!` if you want mutable access to the context while iterating
fn iter_values<'c, Var: IntVar>(
    context: &'c PropagationContext,
    var: &'c PropagatorVariable<Var>,
) -> impl Iterator<Item = i32> + 'c {
    (context.lower_bound(var)..=context.upper_bound(var)).filter(|i| context.contains(var, *i))
}

/// Helper to loop through the domain values of an IntVar without keeping a reference to the context
macro_rules! for_domain_values {
    ($context:expr, $var:expr, |$val:ident| $body:expr) => {
        for $val in ($context.lower_bound($var)..=$context.upper_bound($var)) {
            if $context.contains($var, $val) {
                $body
            }
        }
    };
}

impl<VX: IntVar + 'static, VI: IntVar + 'static, VE: IntVar + 'static> CPPropagatorConstructor
    for Element<VX, VI, VE>
{
    type Args = ElementArgs<VX, VI, VE>;

    fn create(
        args: Self::Args,
        mut context: PropagatorConstructorContext<'_>,
    ) -> Box<dyn ConstraintProgrammingPropagator> {
        // local ids of array vars are shifted by ID_X_OFFSET
        let array = args
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
        Box::new(Element {
            array,
            index: context.register(args.index, DomainEvents::ANY_INT, ID_INDEX),
            rhs: context.register(args.rhs, DomainEvents::ANY_INT, ID_RHS),
            propagations_index: HashMap::new(),
            propagations_rhs: HashMap::new(),
        })
    }
}

impl<VX: IntVar, VI: IntVar, VE: IntVar> Element<VX, VI, VE> {
    /// Helper for `<Element as ConstraintProgrammingPropagator>::get_reason_for_propagation`
    fn interpret_delta(&self, delta: Delta) -> (LocalId, DomainChange) {
        let id = delta.affected_local_id();
        let dc = match id {
            ID_INDEX => self.index.unpack(delta),
            ID_RHS => self.rhs.unpack(delta),
            i => self.array[(i.unpack() - ID_X_OFFSET) as usize].unpack(delta),
        };
        (id, dc)
    }
}

impl<VX: IntVar, VI: IntVar, VE: IntVar> ConstraintProgrammingPropagator for Element<VX, VI, VE> {
    fn propagate(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        // For incremental solving: use the doubly linked list data-structure
        if context.is_fixed(&self.index) {
            // At this point, we should post x_i = e as a new constraint, but that's not an option
            //  in Pumpkin right now. So instead we manually make them equal
            let i = context.lower_bound(&self.index);
            let x_i = &self.array[i as usize];

            // place to put reason if necessary
            let rhs_reason = OnceCell::new();

            let lb = min(context.lower_bound(&self.rhs), context.lower_bound(x_i));
            let ub = max(context.upper_bound(&self.rhs), context.upper_bound(x_i));

            context.set_lower_bound(&self.rhs, lb)?;
            context.set_lower_bound(x_i, lb)?;
            context.set_upper_bound(&self.rhs, ub)?;
            context.set_upper_bound(x_i, ub)?;

            for v in lb..=ub {
                if !context.contains(&self.rhs, v) && context.contains(x_i, v) {
                    context.remove(x_i, v)?;
                } else if context.contains(&self.rhs, v) && !context.contains(x_i, v) {
                    self.propagations_rhs.insert(
                        v,
                        // N.B. rhs_reason is loop-independent
                        Rc::clone(rhs_reason.get_or_init(|| {
                            SliceWithHeader::new(predicate![self.index == i].into(), [i])
                        })),
                    );
                    context.remove(&self.rhs, v)?;
                }
            }
        } else {
            // Remove values from i when for no values of e: x_i = e
            let index_reason = OnceCell::new();
            for_domain_values!(context, &self.index, |i| {
                let x_i = &self.array[i as usize];
                if !iter_values(context, &self.rhs).any(|e| context.contains(x_i, e)) {
                    self.propagations_index.insert(
                        i,
                        // N.B. index_reason is loop-independent
                        Rc::clone(index_reason.get_or_init(|| {
                            SliceWithHeader::new(
                                context.describe_domain(&self.rhs).into(),
                                iter_values(context, &self.rhs).collect::<Vec<_>>(),
                            )
                        })),
                    );
                    context.remove(&self.index, i)?;
                }
            });

            // Remove values from e when for no values of i: x_i = e
            let rhs_reason = OnceCell::new();
            for_domain_values!(context, &self.rhs, |e| {
                if !iter_values(context, &self.index)
                    .map(|i| &self.array[i as usize])
                    .any(|x_i| context.contains(x_i, e))
                {
                    self.propagations_rhs.insert(
                        e,
                        // N.B. rhs_reason is loop-independent
                        Rc::clone(rhs_reason.get_or_init(|| {
                            SliceWithHeader::new(
                                context.describe_domain(&self.index).into(),
                                iter_values(context, &self.index).collect::<Vec<_>>(),
                            )
                        })),
                    );
                    context.remove(&self.rhs, e)?;
                }
            });
        }
        Ok(())
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        // clean up old reason information
        self.propagations_rhs
            .retain(|&k, _| !context.contains(&self.rhs, k));
        self.propagations_index
            .retain(|&k, _| !context.contains(&self.index, k));
    }

    fn get_reason_for_propagation(
        &mut self,
        context: &PropagationContext,
        delta: Delta,
    ) -> PropositionalConjunction {
        match self.interpret_delta(delta) {
            (ID_INDEX, DomainChange::Removal(i)) => {
                let x_i = &self.array[i as usize];
                let reason_info = self.propagations_index.get(&i).unwrap();
                let mut reason = reason_info.header.clone();
                reason_info
                    .slice
                    .iter()
                    .for_each(|e| reason.and(predicate![x_i != *e]));
                reason
            }
            (ID_INDEX, _) => unreachable!(),
            (ID_RHS, DomainChange::Removal(e)) => {
                let reason_info = self.propagations_rhs.get(&e).unwrap();
                let mut reason = reason_info.header.clone();
                reason_info
                    .slice
                    .iter()
                    .for_each(|i| reason.and(predicate![self.array[*i as usize] != e]));
                reason
            }
            (ID_RHS, DomainChange::UpperBound(v)) => {
                debug_assert!(context.is_fixed(&self.index));
                let i = context.lower_bound(&self.index);
                let x_i = &self.array[i as usize];
                conjunction![[self.index == i] & [x_i <= v]]
            }
            (ID_RHS, DomainChange::LowerBound(v)) => {
                debug_assert!(context.is_fixed(&self.index));
                let i = context.lower_bound(&self.index);
                let x_i = &self.array[i as usize];
                conjunction![[self.index == i] & [x_i >= v]]
            }
            (id, DomainChange::Removal(v)) => {
                let i = id.unpack() - ID_X_OFFSET;
                debug_assert!(context.is_fixed(&self.index));
                debug_assert_eq!(i, context.lower_bound(&self.index) as u32);

                let i = i as i32;
                conjunction![[self.index == i] & [self.rhs != v]]
            }
            (id, DomainChange::UpperBound(v)) => {
                let i = id.unpack() - ID_X_OFFSET;
                debug_assert!(context.is_fixed(&self.index));
                debug_assert_eq!(i, context.lower_bound(&self.index) as u32);

                let i = i as i32;
                conjunction![[self.index == i] & [self.rhs <= v]]
            }
            (id, DomainChange::LowerBound(v)) => {
                let i = id.unpack() - ID_X_OFFSET;
                debug_assert!(context.is_fixed(&self.index));
                debug_assert_eq!(i, context.lower_bound(&self.index) as u32);

                let i = i as i32;
                conjunction![[self.index == i] & [self.rhs >= v]]
            }
            (_, _) => unreachable!(),
        }
    }

    fn priority(&self) -> u32 {
        // Priority higher than int_times/linear_eq/not_eq_propagator because it's much more
        //  expensive looping over multiple domains
        2
    }

    fn name(&self) -> &str {
        "Element"
    }

    fn initialise_at_root(&mut self, context: &mut PropagationContext) -> PropagationStatusCP {
        // Ensure index is non-negative
        context.set_lower_bound(&self.index, 0)?;
        // Ensure index <= no. of x_j
        context.set_upper_bound(&self.index, self.array.len() as i32)?;

        self.propagate(context)
    }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContext,
    ) -> PropagationStatusCP {
        // Close to duplicate of `propagate` for now, without saving reason stuff...
        if context.is_fixed(&self.index) {
            let i = context.lower_bound(&self.index);
            let x_i = &self.array[i as usize];

            let lb = min(context.lower_bound(&self.rhs), context.lower_bound(x_i));
            let ub = max(context.upper_bound(&self.rhs), context.upper_bound(x_i));

            context.set_lower_bound(&self.rhs, lb)?;
            context.set_lower_bound(x_i, lb)?;
            context.set_upper_bound(&self.rhs, ub)?;
            context.set_upper_bound(x_i, ub)?;

            for_domain_values!(context, x_i, |e| {
                if !context.contains(&self.rhs, e) {
                    context.remove(x_i, e)?;
                }
            });
            for_domain_values!(context, &self.rhs, |e| {
                if !context.contains(x_i, e) {
                    context.remove(&self.rhs, e)?;
                }
            });
        } else {
            // Remove values from i when for no values of e: x_i = e
            for_domain_values!(context, &self.index, |i| {
                let x_i = &self.array[i as usize];
                if !iter_values(context, &self.rhs).any(|e| context.contains(x_i, e)) {
                    context.remove(&self.index, i)?;
                }
            });
            // Remove values from e when for no values of i: x_i = e
            for_domain_values!(context, &self.rhs, |e| {
                if !iter_values(context, &self.index)
                    .map(|i| &self.array[i as usize])
                    .any(|x_i| context.contains(x_i, e))
                {
                    context.remove(&self.rhs, e)?;
                }
            });
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::conjunction;
    use crate::engine::test_helper::TestSolver;

    use super::*;

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
            .new_propagator::<Element<_, _, _>>(ElementArgs { array, index, rhs })
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
            .new_propagator::<Element<_, _, _>>(ElementArgs { array, index, rhs })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        let index_delta = Delta::new(ID_INDEX, DomainChange::Removal(3));
        let rhs_delta = Delta::new(ID_RHS, DomainChange::Removal(6));

        let index_reason = solver.get_reason(&mut propagator, index_delta);
        let rhs_reason = solver.get_reason(&mut propagator, rhs_delta);

        // reason for index removal 3 is that `x_3 != e`
        assert_eq!(
            index_reason,
            conjunction!(
                [rhs >= 6] & [rhs <= 9] & [x_3 != 6] & [x_3 != 7] & [x_3 != 8] & [x_3 != 9]
            )
        );
        // reason for rhs removal 6 is that for all valid indices i `x_i != 6`
        assert_eq!(
            rhs_reason,
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
            .new_propagator::<Element<_, _, _>>(ElementArgs { array, index, rhs })
            .expect("no empty domains");

        solver.propagate(&mut propagator).expect("no empty domains");

        let x1_ub_delta = Delta::new(LocalId::from(ID_X_OFFSET + 1), DomainChange::UpperBound(9));
        let x1_8_delta = Delta::new(LocalId::from(ID_X_OFFSET + 1), DomainChange::Removal(8));
        let rhs_delta = Delta::new(ID_RHS, DomainChange::LowerBound(7));

        let x1_ub_reason = solver.get_reason(&mut propagator, x1_ub_delta);
        let x1_8_reason = solver.get_reason(&mut propagator, x1_8_delta);
        let rhs_reason = solver.get_reason(&mut propagator, rhs_delta);

        // reason for x_1 <= 9 is that `x_1 == e` and `e <= 9`
        assert_eq!(x1_ub_reason, conjunction!([index == 1] & [rhs <= 9]));
        // reason for x_1 removal 8 is that `x_1 == e` and `e != 8`
        assert_eq!(x1_8_reason, conjunction!([index == 1] & [rhs != 8]));
        // reason for `rhs >= 7` is that `x_1 >= 7`
        assert_eq!(rhs_reason, conjunction!([index == 1] & [x_1 >= 7]));
    }
}
