use std::ops::Not;

use log::warn;

use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashSet;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::StorageKey;
use crate::conjunction;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::ReadDomains;
use crate::pumpkin_assert_simple;

// TODO:
// Define data structures.
// Notify should accumulate things to propagate.
// Propagating goes through the data structures?
//
// todo: for now we do not accumulate the updates, we simply update as we enqueued them.
// todo: could specialise the propagator for binary integers. This is a general comment.

// Need also LBD, and nogood clean ups! Remove root level true predicates.

// Todo: add predicate compression for 1) logging nogoods, and 2) smaller memory footprint.

// Current data structure:
// [domain_id][operation] -> [list of A], where A is list of watchers, where a watcher where is:

// Say for operation >=
// A is a list of pairs (bound, nogood)

#[derive(Debug, Clone, Copy)]
pub struct NogoodPropagatorConstructor {}

impl PropagatorConstructor for NogoodPropagatorConstructor {
    type Propagator = NogoodPropagator;

    fn create(self, mut _context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        NogoodPropagator::default()
    }
}

// #[derive(Default, Clone, Debug)]
// struct Watcher {
// This field represents the right-hand side of the predicate present in the nogood.
// It is used as an indicator to whether the nogood should be inspected.
// right_hand_side: i32,
// nogood_id: NogoodId,
// todo: consider the cached literal
// }
//
// #[derive(Default, Clone, Debug)]
// struct WatchList {
// nogoods with a watched predicate [x >= k]
// lower_bound: Vec<Watcher>,
// nogoods with a watched predicate [x <= k]
// upper_bound: Vec<Watcher>,
// nogoods with a watched predicate [x != k]
// hole: Vec<Watcher>,
// nogoods with a watched predicate [x == k]
// assignment: Vec<Watcher>,
// }
//
// #[derive(Clone, Debug)]
// enum Update {
// LowerBound {
// old_lower_bound: i32,
// new_lower_bound: i32,
// },
// UpperBound {
// old_upper_bound: i32,
// new_upper_bound: i32,
// },
// Hole {
// hole_value: i32,
// },
// Assignment {
// assigned_value: i32,
// },
// }

#[derive(Default, Clone, Debug)]
struct Nogood {
    predicates: Vec<IntegerPredicate>,
    #[allow(dead_code)]
    is_learned: bool,
}

#[derive(Default, Clone, Debug)]
pub struct NogoodPropagator {
    #[allow(dead_code)]
    nogoods: Vec<Nogood>,
    permanent_nogoods: Vec<NogoodId>,
    learned_nogoods: Vec<NogoodId>,
    is_in_infeasible_state: bool,
    // Watch lists for the nogood propagator.
    // todo: could improve the data structure for watching.
    // watch_lists: HashMap<DomainId, WatchList>,
    // enqueued_updates: Vec<Update>,
}

impl NogoodPropagator {
    /// Does simple preprocessing, modifying the input nogood by:
    ///     1. Removing duplicate predicates.
    ///     2. Removing satisfied predicates at the root.
    ///     3. Detecting predicates falsified at the root. In that case, the nogood is preprocessed
    ///        to the empty nogood.
    ///     4. Conflicting predicates?
    fn preprocess_nogood(nogood: &mut Vec<IntegerPredicate>, context: &mut PropagationContextMut) {
        pumpkin_assert_simple!(context.get_decision_level() == 0);
        // The code below is broken down into several parts,
        // could be done more efficiently but probably okay.

        // Check if the nogood cannot be violated, i.e., it has a falsified predicate.
        if nogood.is_empty() || nogood.iter().any(|p| context.is_predicate_falsified(*p)) {
            nogood.clear();
            return;
        }

        // Remove predicates that are satisfied at the root level.
        nogood.retain(|p| !context.is_predicate_satisfied(*p));

        // If the nogood is violating at the root, the previous retain would leave an empty
        // Return a violating nogood.
        if nogood.is_empty() {
            *nogood = vec![IntegerPredicate::trivially_true()];
            return;
        }

        // We now remove duplicated predicates.
        let mut present_predicates: HashSet<IntegerPredicate> = HashSet::default();
        // We make use that adding elements to a hashset returns true if the element was not present
        // in the set.
        nogood.retain(|p| present_predicates.insert(*p));

        // Check for contradicting predicates. In this case, the nogood cannot lead to propagation,
        // so we can ignore it.
        // Todo: The current version only partially does this, since there may be symmetries that
        // are not detected, e.g., for a 0-1 integer variable, [x >= 1] and [x == 0] are
        // opposite predicates but we do not detect these, and only check for direct
        // negatives [x <= 0] and [x == 1].
        if nogood.iter().any(|p| present_predicates.contains(&p.not())) {
            nogood.clear();
        }
    }

    #[allow(dead_code)]
    fn add_permanent_nogood(
        &mut self,
        mut nogood: Vec<IntegerPredicate>,
        context: &mut PropagationContextMut,
    ) -> Result<(), ConstraintOperationError> {
        pumpkin_assert_simple!(
            context.get_decision_level() == 0,
            "Only allowed to add nogoods permanently at the root for now."
        );

        if self.is_in_infeasible_state {
            return Err(ConstraintOperationError::InfeasibleState);
        }

        if nogood.is_empty() {
            warn!("Adding empty nogood, unusual!");
            return Ok(());
        }

        Self::preprocess_nogood(&mut nogood, context);

        // Unit nogoods are added as root assignments rather than as nogoods.
        if nogood.len() == 1 {
            if context.is_predicate_satisfied(nogood[0]) {
                self.is_in_infeasible_state = true;
                Err(ConstraintOperationError::InfeasibleNogood)
            } else if context.is_predicate_falsified(nogood[0]) {
                Ok(())
            } else {
                // Post the negated predicate at the root to respect the nogood.
                let result = context.post_predicate(nogood[0], conjunction!());
                match result {
                    Ok(_) => Ok(()),
                    Err(_) => {
                        self.is_in_infeasible_state = true;
                        Err(ConstraintOperationError::InfeasibleNogood)
                    }
                }
            }
        }
        // Standard case, nogood is of size at least two.
        // The preprocessing ensures that all predicates are unassigned.
        else {
            let new_nogood_id = NogoodId {
                id: self.permanent_nogoods.len() as u32,
            };
            self.permanent_nogoods.push(new_nogood_id);
            self.nogoods.push(Nogood {
                predicates: nogood,
                is_learned: false,
            });
            Ok(())
        }
    }

    #[allow(dead_code)]
    fn is_propagation_complete(&self, _trail_size: usize) -> bool {
        todo!();
    }

    #[allow(dead_code)]
    fn debug_propagate_nogood_from_scratch(
        &self,
        nogood: &[IntegerPredicate],
        context: &mut PropagationContextMut,
    ) -> Result<(), Inconsistency> {
        // Inefficient way of propagating, but okay for testing purposes
        // Explicitly goes through each predicate, and does multiple passes.

        let num_falsified_predicates = nogood
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| !x))
            .count();

        // if at least one predicate is false, then the nogood can be skipped
        if num_falsified_predicates > 0 {
            return Ok(());
        }

        let num_satisfied_predicates = nogood
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| x))
            .count();

        assert!(num_satisfied_predicates + num_falsified_predicates <= nogood.len());

        // If all predicates in the nogood are satisfied, there is a conflict.
        if num_satisfied_predicates == nogood.len() {
            return Err(Inconsistency::Conflict {
                conflict_nogood: nogood.iter().copied().collect(),
            });
        }
        // If all but one predicate are satisfied, then we can propagate.
        // Note that this only makes sense since we know that there are no falsifying predicates at
        // this point.
        else if num_satisfied_predicates == nogood.len() - 1 {
            // Note that we negate the remaining unassigned predicate!
            let propagated_predicate = nogood
                .iter()
                .find(|predicate| context.evaluate_predicate(**predicate).is_none())
                .unwrap()
                .not();
            assert!(nogood.iter().any(|p| *p == propagated_predicate.not()));

            let reason: PropositionalConjunction = nogood
                .iter()
                .filter(|p| **p != propagated_predicate.not())
                .copied()
                .collect();
            context.post_predicate(propagated_predicate, reason)?;
        }
        Ok(())
    }

    // fn add_watcher(&mut self, predicate: IntegerPredicate, nogood_id: NogoodId) {
    // todo: look up Hashset operations to do this properly.
    // Create an entry in case the predicate is missing
    // if !self.watch_lists.contains_key(&predicate.get_domain()) {
    // let _ = self
    // .watch_lists
    // .insert(predicate.get_domain(), WatchList::default());
    // }
    //
    // match predicate {
    // IntegerPredicate::LowerBound {
    // domain_id,
    // lower_bound,
    // } => todo!(),
    // IntegerPredicate::UpperBound {
    // domain_id,
    // upper_bound,
    // } => todo!(),
    // IntegerPredicate::NotEqual {
    // domain_id,
    // not_equal_constant,
    // } => todo!(),
    // IntegerPredicate::Equal {
    // domain_id,
    // equality_constant,
    // } => todo!(),
    // }
    //
    //         self.watch_lists
    // .get_mut(&predicate.get_domain())
    // .unwrap()
    // .push(Watcher { nogood_id });
    // }
}

impl Propagator for NogoodPropagator {
    fn name(&self) -> &str {
        // It is important to keep this name exactly this.
        // In parts of code for debugging, it looks for this particular name.
        "NogoodPropagator"
    }

    fn priority(&self) -> u32 {
        0
    }

    // fn propagate(&mut self, context: &mut PropagationContextMut) -> Result<(), Inconsistency> {
    // todo: do incrementally, two levels:
    // 1. Skip looking at nogoods that are for sure not changing their watched predicate.
    // For this need to take into account the trail position and when the updates took place.
    // 2. While going through the watch list, partition the watchers. Then next time we can skip
    // the ones we looked at before, since those are guaranteed to be not propagating. I think
    // this only works downwards, need to see how to handle synchronisation/backtracking.
    //
    // The idea is to go through nogoods which had their watched predicate evaluate to true.
    // for update in &self.enqueued_updates {
    // match update {
    // Update::LowerBound {
    // old_lower_bound,
    // new_lower_bound,
    // } => {
    // Go through the watchers of:
    // 1) Lower bounds in the range (old, new].
    // 2) Not equals in the range [old, new).
    // Equality watchers can be skipped since that is a separate domain event.
    // todo!()
    // }
    // Update::UpperBound {
    // old_upper_bound,
    // new_upper_bound,
    // } => {
    // Go through the watchers of:
    // 1) Upper bounds in the range [new, old).
    // 2) Not equals in the range (new, old].
    // Equality watchers can be skipped since that is a separate domain event.
    // todo!()
    // }
    // Update::Hole { hole_value } => {
    // Only need to go through the watchers of
    // not equals for exactly the hole value.
    // todo!()
    // }
    // Update::Assignment { assigned_value } => {
    // Only need to go through the watchers of
    // assignments for exactly the assigned value.
    // todo!()
    // }
    // }
    // }
    // todo!();
    // }

    // fn synchronise(&mut self, _context: &PropagationContext) {
    // self.enqueued_updates.clear();
    // }

    // fn notify(
    // &mut self,
    // _context: &mut PropagationContextMut,
    // _local_id: LocalId,
    // _event: OpaqueDomainEvent,
    // ) -> EnqueueDecision {
    // self.enqueued_updates.push(value)
    // EnqueueDecision::Enqueue
    // It seems we removed the delta idea?
    // todo!();
    // }

    fn debug_propagate_from_scratch(
        &self,
        context: &mut PropagationContextMut,
    ) -> Result<(), Inconsistency> {
        // Very inefficient version!

        // The algorithm goes through every nogood explicitly
        // and computes from scratch.
        for nogood in self.nogoods.iter() {
            self.debug_propagate_nogood_from_scratch(&nogood.predicates, context)?;
        }
        Ok(())
    }

    // Learned nogood during search.
    // Assumption is that it is propagating, and that the propagating predicate is in position [0].
    // Actually the above assumption is not needed in the current version.
    #[allow(dead_code)]
    fn hack_add_asserting_nogood(
        &mut self,
        nogood: Vec<IntegerPredicate>,
        context: &mut PropagationContextMut,
    ) {
        self.debug_propagate_nogood_from_scratch(&nogood, context)
            .expect("Do not expect to fail propagating learned nogood.");
        let nogood_id = NogoodId {
            id: self.nogoods.len() as u32,
        };
        self.nogoods.push(Nogood {
            predicates: nogood,
            is_learned: true,
        });
        self.learned_nogoods.push(nogood_id);

        // self.nogoods.push(nogood);
        // let nogood_id = NogoodId {
        // id: self.nogoods.len() as u32,
        // };
        // self.add_watcher(nogood[0], nogood_id);
        // self.add_watcher(nogood[1], nogood_id);
        // self.nogoods.push(nogood);
        //
        // TODO PROPAGATION
        // todo!();
    }

    /// Temporary hack, used to add nogoods. Will be replaced later.
    fn hack_add_nogood(
        &mut self,
        nogood: Vec<IntegerPredicate>,
        context: &mut PropagationContextMut,
    ) -> Result<(), ConstraintOperationError> {
        match self.add_permanent_nogood(nogood, context) {
            Ok(_) => Ok(()),
            Err(e) => {
                self.is_in_infeasible_state = true;
                Err(e)
            }
        }
    }
}

#[derive(Default, Clone, Copy, Debug)]
struct NogoodId {
    id: u32,
}

impl StorageKey for NogoodId {
    fn index(&self) -> usize {
        self.id as usize
    }

    fn create_from_index(index: usize) -> Self {
        NogoodId { id: index as u32 }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    // use crate::conjunction;
    // use crate::engine::test_helper::TestSolver;
    // use crate::predicate;

    // detect unsat
    // detect propagation correctly
    //
    // check reason for unsat
    // check reason for propagation
    //
    // inconsistent nogood -> never propagates
    //
    // nogood with redundant predicate

    #[test]
    fn detect_unsat() {
        // let mut solver = TestSolver::default();
        // let a = solver.new_variable(1, 3);
        // let b = solver.new_variable(0, 4);
        // let c = solver.new_variable(-10, 20);
        //
        // let mut propagator = solver
        // .new_propagator(NogoodPropagator { a, b, c })
        // .expect("no empty domains");
        // solver.propagate(&mut propagator).expect("no empty domains");
        //
        // assert_eq!(1, solver.lower_bound(a));
        // assert_eq!(3, solver.upper_bound(a));
        // assert_eq!(0, solver.lower_bound(b));
        // assert_eq!(4, solver.upper_bound(b));
        // assert_eq!(0, solver.lower_bound(c));
        // assert_eq!(12, solver.upper_bound(c));
        //
        // let reason_lb = solver.get_reason_int(predicate![c >= 0]);
        // assert_eq!(conjunction!([a >= 0] & [b >= 0]), *reason_lb);
        //
        // let reason_ub = solver.get_reason_int(predicate![c <= 12]);
        // assert_eq!(
        // conjunction!([a >= 0] & [a <= 3] & [b >= 0] & [b <= 4]),
        // reason_ub
        // );
    }
}
