use std::ops::Not;

use log::warn;

use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashMap;
use crate::basic_types::HashSet;
use crate::basic_types::Inconsistency;
use crate::basic_types::KeyedVec;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::StorageKey;
use crate::conjunction;
use crate::engine::conflict_analysis::AdvancedNogood;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::propagation::propagation_context::HasAssignments;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::ReadDomains;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
use crate::engine::EventSink;
use crate::engine::IntDomainEvent;
use crate::pumpkin_assert_moderate;
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

// Todo: the way the hashmap is used is not efficient.

#[derive(Debug, Clone, Copy)]
pub struct NogoodPropagatorConstructor {}

impl PropagatorConstructor for NogoodPropagatorConstructor {
    type Propagator = NogoodPropagator;

    fn create(self, mut _context: PropagatorConstructorContext<'_>) -> Self::Propagator {
        NogoodPropagator::default()
    }
}

#[derive(Default, Clone, Debug)]
struct Nogood {
    predicates: Vec<IntegerPredicate>,
    #[allow(dead_code)]
    is_learned: bool,
}

#[derive(Clone, Copy, Debug, Hash)]
struct PairDomainEvent {
    domain_id: DomainId,
    event: IntDomainEvent,
}

#[derive(Default, Clone, Debug)]
pub struct NogoodPropagator {
    nogoods: KeyedVec<NogoodId, Nogood>,
    permanent_nogoods: Vec<NogoodId>,
    learned_nogoods: Vec<NogoodId>,
    // The trail index is used to determine the domains of the variables since last time.
    last_index_on_trail: usize,
    is_in_infeasible_state: bool,
    // Watch lists for the nogood propagator.
    // todo: could improve the data structure for watching.
    watch_lists: HashMap<DomainId, WatchList>,
    enqueued_updates: EventSink, /* HashSet<(DomainId, IntDomainEvent)>, // Vec<(DomainId,
                                  * IntDomainEvent)>, */
    potential_empty_lists: Vec<DomainId>,
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
            *nogood = vec![IntegerPredicate::trivially_false()];
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
        // negatives [x <= 0] and [x == 1], [x == 1] & [x == 2] is not sensible.
        if nogood.iter().any(|p| present_predicates.contains(&p.not())) {
            *nogood = vec![IntegerPredicate::trivially_false()];
        }

        // This is a way to do semantic minimisation.
        let mut temp = AdvancedNogood::new(0);
        temp.add_predicates(nogood.clone(), context.assignments, None);
        *nogood = temp.extract_final_learned_nogood().predicates;
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
                let result = context.post_predicate(!nogood[0], conjunction!());
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
                id: self.nogoods.len() as u32,
            };
            self.permanent_nogoods.push(new_nogood_id);

            self.add_watcher(nogood[0], new_nogood_id);
            self.add_watcher(nogood[1], new_nogood_id);

            self.nogoods.push(Nogood {
                predicates: nogood,
                is_learned: false,
            });
            Ok(())
        }
    }

    fn clean_up_entry_in_watch_list_if_needed(&mut self, domain_id: DomainId) {
        if let Some(wl) = self.watch_lists.get(&domain_id) {
            let empty_watch_list = wl.lower_bound.is_empty()
                && wl.upper_bound.is_empty()
                && wl.hole.is_empty()
                && wl.equals.is_empty();
            if empty_watch_list {
                let _ = self.watch_lists.remove(&domain_id);
            }
        }
    }

    fn add_watcher(&mut self, predicate: IntegerPredicate, nogood_id: NogoodId) {
        // Add this nogood to the watch list of the new watcher.
        // Ensure there is an entry.
        #[allow(clippy::map_entry)]
        if !self.watch_lists.contains_key(&predicate.get_domain()) {
            let _ = self
                .watch_lists
                .insert(predicate.get_domain(), WatchList::default());
        }
        match predicate {
            IntegerPredicate::LowerBound {
                domain_id,
                lower_bound,
            } => self
                .watch_lists
                .get_mut(&domain_id)
                .unwrap()
                .lower_bound
                .push(Watcher {
                    right_hand_side: lower_bound,
                    nogood_id,
                }),
            IntegerPredicate::UpperBound {
                domain_id,
                upper_bound,
            } => self
                .watch_lists
                .get_mut(&domain_id)
                .unwrap()
                .upper_bound
                .push(Watcher {
                    right_hand_side: upper_bound,
                    nogood_id,
                }),
            IntegerPredicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self
                .watch_lists
                .get_mut(&domain_id)
                .unwrap()
                .hole
                .push(Watcher {
                    right_hand_side: not_equal_constant,
                    nogood_id,
                }),
            IntegerPredicate::Equal {
                domain_id,
                equality_constant,
            } => self
                .watch_lists
                .get_mut(&domain_id)
                .unwrap()
                .equals
                .push(Watcher {
                    right_hand_side: equality_constant,
                    nogood_id,
                }),
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

            println!("from scratching: {}", propagated_predicate);
            println!("...because of: {:?}", reason);

            context.post_predicate(propagated_predicate, reason)?;
        }
        Ok(())
    }

    // fn is_update_present(&self, local_id: LocalId, event: OpaqueDomainEvent) -> bool {
    // self.enqueued_updates.contains(&(
    // DomainId {
    // id: local_id.unpack(),
    // },
    // event.unwrap(),
    // ))
    // }

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

    #[allow(dead_code)]
    fn debug_print(&self, assignments: &Assignments) {
        for nogood in self.nogoods.iter() {
            print!("ng: ");

            for p in nogood.predicates.iter() {
                let m = match assignments.evaluate_predicate(*p) {
                    Some(b) => b as i32,
                    None => -1,
                };

                print!("{} ({}) ", p, m);
            }
            println!();
        }

        println!("eq list");
        for m in &self.watch_lists {
            println!("var {}: {:?}", m.0, m.1.equals);
        }
        println!("+++++++");
    }
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

    fn propagate(&mut self, context: &mut PropagationContextMut) -> Result<(), Inconsistency> {
        // old version from scratch:
        // let result = self.debug_propagate_from_scratch(context);
        // self.last_index_on_trail = context.assignments.trail.len();
        // return result;

        for t in context.assignments.trail.iter() {
            println!("\t{}, dec: {}", t.predicate, t.reason.is_none());
        }

        println!("before propagate");
        self.debug_print(context.assignments());

        for update_info in self.enqueued_updates.drain() {
            let update_info = (update_info.1, update_info.0);
            println!("\tmm: {}", update_info.0);
            if let Some(hehe) = self.watch_lists.get(&update_info.0) {
                for m in hehe.equals.iter() {
                    println!("w {}: {:?}", m.right_hand_side, self.nogoods[m.nogood_id]);
                }
            }
            match update_info.1 {
                IntDomainEvent::Assign => println!("ass"),
                IntDomainEvent::LowerBound => println!("lb"),
                IntDomainEvent::UpperBound => println!("ub"),
                IntDomainEvent::Removal => println!("rem"),
            }

            match update_info.1 {
                IntDomainEvent::LowerBound => {
                    let old_lower_bound = context
                        .lower_bound_at_trail_position(&update_info.0, self.last_index_on_trail);
                    let new_lower_bound = context.lower_bound(&update_info.0);

                    // We are manually implementing a retain-like function from Vec.

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let mut current_index = 0_usize;
                    let mut end_index = 0_usize;
                    let num_watchers = self
                        .watch_lists
                        .get(&update_info.0)
                        .map(|wl| wl.lower_bound.len())
                        .unwrap_or(0);
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side = self
                            .watch_lists
                            .get_mut(&update_info.0)
                            .unwrap()
                            .lower_bound[current_index]
                            .right_hand_side;

                        if old_lower_bound < right_hand_side && right_hand_side <= new_lower_bound {
                            // todo: check cached predicate?

                            let nogood_id = self
                                .watch_lists
                                .get_mut(&update_info.0)
                                .unwrap()
                                .lower_bound[current_index]
                                .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: IntegerPredicate| {
                                predicate.is_lower_bound_predicate()
                                    && predicate.get_domain() == update_info.0
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            // todo: check if comparing to the cache literal would make sense.
                            if context.is_predicate_falsified(nogood[0]) {
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .lower_bound[end_index] = self
                                    .watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .lower_bound[current_index];
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;
                                    // todo: does it make sense to replace the cached predicate with
                                    // this new predicate?

                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == update_info.0
                                    );
                                    self.potential_empty_lists.push(nogood[i].get_domain());
                                    // Add this nogood to the watch list of the new watcher.
                                    // Ensure there is an entry.
                                    #[allow(clippy::map_entry)]
                                    if !self.watch_lists.contains_key(&nogood[1].get_domain()) {
                                        let _ = self
                                            .watch_lists
                                            .insert(nogood[1].get_domain(), WatchList::default());
                                    }
                                    match nogood[1] {
                                        IntegerPredicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .lower_bound
                                            .push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .upper_bound
                                            .push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => {
                                            self.watch_lists.get_mut(&domain_id).unwrap().hole.push(
                                                Watcher {
                                                    right_hand_side: not_equal_constant,
                                                    nogood_id,
                                                },
                                            )
                                        }
                                        IntegerPredicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .equals
                                            .push(Watcher {
                                                right_hand_side: equality_constant,
                                                nogood_id,
                                            }),
                                    }

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                // Note this nogood is effectively removed from the watch list
                                // of the the current predicate, since we
                                // are only incrementing the current index, and not copying
                                // anything to the end_index.

                                current_index += 1;
                                continue;
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists
                                .get_mut(&update_info.0)
                                .unwrap()
                                .lower_bound[end_index] = self
                                .watch_lists
                                .get_mut(&update_info.0)
                                .unwrap()
                                .lower_bound[current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason: PropositionalConjunction =
                                nogood[1..].iter().copied().collect();
                            pumpkin_assert_moderate!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));
                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists
                                        .get_mut(&update_info.0)
                                        .unwrap()
                                        .lower_bound[end_index] = self
                                        .watch_lists
                                        .get_mut(&update_info.0)
                                        .unwrap()
                                        .lower_bound[current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .lower_bound
                                    .truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists
                            .get_mut(&update_info.0)
                            .unwrap()
                            .lower_bound
                            .truncate(end_index);
                    }
                }
                IntDomainEvent::UpperBound => {
                    let old_upper_bound = context
                        .upper_bound_at_trail_position(&update_info.0, self.last_index_on_trail);
                    let new_upper_bound = context.upper_bound(&update_info.0);

                    // We are manually implementing a retain-like function from Vec.

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let mut current_index = 0_usize;
                    let mut end_index = 0_usize;
                    let num_watchers = self
                        .watch_lists
                        .get(&update_info.0)
                        .map(|wl| wl.upper_bound.len())
                        .unwrap_or(0);
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side = self
                            .watch_lists
                            .get_mut(&update_info.0)
                            .unwrap()
                            .upper_bound[current_index]
                            .right_hand_side;

                        if old_upper_bound > right_hand_side && right_hand_side >= new_upper_bound {
                            // todo: check cached predicate?

                            let nogood_id = self
                                .watch_lists
                                .get_mut(&update_info.0)
                                .unwrap()
                                .upper_bound[current_index]
                                .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: IntegerPredicate| {
                                predicate.is_upper_bound_predicate()
                                    && predicate.get_domain() == update_info.0
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            // todo: check if comparing to the cache literal would make sense.
                            if context.is_predicate_falsified(nogood[0]) {
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .upper_bound[end_index] = self
                                    .watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .upper_bound[current_index];
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;
                                    // todo: does it make sense to replace the cached predicate with
                                    // this new predicate?

                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == update_info.0
                                    );
                                    self.potential_empty_lists.push(nogood[i].get_domain());
                                    // Add this nogood to the watch list of the new watcher.
                                    // Ensure there is an entry.
                                    #[allow(clippy::map_entry)]
                                    if !self.watch_lists.contains_key(&nogood[1].get_domain()) {
                                        let _ = self
                                            .watch_lists
                                            .insert(nogood[1].get_domain(), WatchList::default());
                                    }
                                    match nogood[1] {
                                        IntegerPredicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .lower_bound
                                            .push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .upper_bound
                                            .push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => {
                                            self.watch_lists.get_mut(&domain_id).unwrap().hole.push(
                                                Watcher {
                                                    right_hand_side: not_equal_constant,
                                                    nogood_id,
                                                },
                                            )
                                        }
                                        IntegerPredicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .equals
                                            .push(Watcher {
                                                right_hand_side: equality_constant,
                                                nogood_id,
                                            }),
                                    }

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                // Note this nogood is effectively removed from the watch list
                                // of the the current predicate, since we
                                // are only incrementing the current index, and not copying
                                // anything to the end_index.

                                current_index += 1;
                                continue;
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists
                                .get_mut(&update_info.0)
                                .unwrap()
                                .upper_bound[end_index] = self
                                .watch_lists
                                .get_mut(&update_info.0)
                                .unwrap()
                                .upper_bound[current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason: PropositionalConjunction =
                                nogood[1..].iter().copied().collect();
                            pumpkin_assert_moderate!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));
                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists
                                        .get_mut(&update_info.0)
                                        .unwrap()
                                        .upper_bound[end_index] = self
                                        .watch_lists
                                        .get_mut(&update_info.0)
                                        .unwrap()
                                        .upper_bound[current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .upper_bound
                                    .truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists
                            .get_mut(&update_info.0)
                            .unwrap()
                            .upper_bound
                            .truncate(end_index);
                    }
                }
                IntDomainEvent::Removal => {
                    let old_lower_bound = context
                        .lower_bound_at_trail_position(&update_info.0, self.last_index_on_trail);
                    let new_lower_bound = context.lower_bound(&update_info.0);

                    let old_upper_bound = context
                        .upper_bound_at_trail_position(&update_info.0, self.last_index_on_trail);
                    let new_upper_bound = context.upper_bound(&update_info.0);

                    // We are manually implementing a retain-like function from Vec.

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let mut current_index = 0_usize;
                    let mut end_index = 0_usize;
                    let num_watchers = self
                        .watch_lists
                        .get(&update_info.0)
                        .map(|wl| wl.hole.len())
                        .unwrap_or(0);
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side =
                            self.watch_lists.get_mut(&update_info.0).unwrap().hole[current_index]
                                .right_hand_side;

                        if old_upper_bound >= right_hand_side && right_hand_side > new_upper_bound
                            || old_lower_bound <= right_hand_side
                                && right_hand_side < new_lower_bound
                            || (new_lower_bound < right_hand_side
                                && right_hand_side < new_upper_bound)
                        {
                            // todo: check cached predicate?

                            let nogood_id = self.watch_lists.get_mut(&update_info.0).unwrap().hole
                                [current_index]
                                .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: IntegerPredicate| {
                                predicate.is_not_equal_predicate()
                                    && predicate.get_domain() == update_info.0
                                    && predicate.get_right_hand_side() == right_hand_side
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            // todo: check if comparing to the cache literal would make sense.
                            if context.is_predicate_falsified(nogood[0]) {
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists.get_mut(&update_info.0).unwrap().hole[end_index] =
                                    self.watch_lists.get_mut(&update_info.0).unwrap().hole
                                        [current_index];
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;
                                    // todo: does it make sense to replace the cached predicate with
                                    // this new predicate?

                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == update_info.0
                                    );
                                    self.potential_empty_lists.push(nogood[i].get_domain());
                                    // Add this nogood to the watch list of the new watcher.
                                    // Ensure there is an entry.
                                    #[allow(clippy::map_entry)]
                                    if !self.watch_lists.contains_key(&nogood[1].get_domain()) {
                                        let _ = self
                                            .watch_lists
                                            .insert(nogood[1].get_domain(), WatchList::default());
                                    }
                                    match nogood[1] {
                                        IntegerPredicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .lower_bound
                                            .push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .upper_bound
                                            .push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => {
                                            self.watch_lists.get_mut(&domain_id).unwrap().hole.push(
                                                Watcher {
                                                    right_hand_side: not_equal_constant,
                                                    nogood_id,
                                                },
                                            )
                                        }
                                        IntegerPredicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .equals
                                            .push(Watcher {
                                                right_hand_side: equality_constant,
                                                nogood_id,
                                            }),
                                    }

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                // Note this nogood is effectively removed from the watch list
                                // of the the current predicate, since we
                                // are only incrementing the current index, and not copying
                                // anything to the end_index.

                                current_index += 1;
                                continue;
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists.get_mut(&update_info.0).unwrap().hole[end_index] =
                                self.watch_lists.get_mut(&update_info.0).unwrap().hole
                                    [current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason: PropositionalConjunction =
                                nogood[1..].iter().copied().collect();
                            pumpkin_assert_moderate!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));
                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists.get_mut(&update_info.0).unwrap().hole
                                        [end_index] =
                                        self.watch_lists.get_mut(&update_info.0).unwrap().hole
                                            [current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .hole
                                    .truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists
                            .get_mut(&update_info.0)
                            .unwrap()
                            .hole
                            .truncate(end_index);
                    }
                }
                IntDomainEvent::Assign => {
                    let new_lower_bound = context.lower_bound(&update_info.0);
                    let new_upper_bound = context.upper_bound(&update_info.0);

                    // println!("\tlb {new_lower_bound}, ub {new_upper_bound}");

                    assert!(new_lower_bound == new_upper_bound);
                    let assigned_value = new_lower_bound;

                    // We are manually implementing a retain-like function from Vec.

                    // Effectively, resizing the watch list to size zero,
                    // and in the loop add some of the old watchers back.
                    let mut current_index = 0_usize;
                    let mut end_index = 0_usize;
                    let num_watchers = self
                        .watch_lists
                        .get(&update_info.0)
                        .map(|wl| wl.equals.len())
                        .unwrap_or(0);
                    // Iterate through all watchers.

                    while current_index < num_watchers {
                        let right_hand_side =
                            self.watch_lists.get_mut(&update_info.0).unwrap().equals[current_index]
                                .right_hand_side;

                        println!("\teq {} = {}", update_info.0, assigned_value);
                        println!("\tassn val: {}", assigned_value);
                        println!("\t rhs: {}", right_hand_side);

                        if assigned_value == right_hand_side {
                            // todo: check cached predicate?

                            let nogood_id =
                                self.watch_lists.get_mut(&update_info.0).unwrap().equals
                                    [current_index]
                                    .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].predicates;

                            let is_watched_predicate = |predicate: IntegerPredicate| {
                                predicate.is_equality_predicate()
                                    && predicate.get_domain() == update_info.0
                                    && predicate.get_right_hand_side() == assigned_value
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            println!("eye: {:?}", nogood);

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            // todo: check if comparing to the cache literal would make sense.
                            if context.is_predicate_falsified(nogood[0]) {
                                println!("!!falsi");
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists.get_mut(&update_info.0).unwrap().equals
                                    [end_index] =
                                    self.watch_lists.get_mut(&update_info.0).unwrap().equals
                                        [current_index];
                                current_index += 1;
                                end_index += 1;
                                continue;
                            }
                            // Look for another nonsatisfied predicate
                            // to replace the watched predicate.
                            let mut found_new_watch = false;
                            // Start from index 2 since we are skipping watched predicates.
                            for i in 2..nogood.len() {
                                // Find a predicate that is either false or unassigned,
                                // i.e., not assigned true.
                                if !context.is_predicate_satisfied(nogood[i]) {
                                    // Found another predicate that can be the watcher.
                                    found_new_watch = true;
                                    // todo: does it make sense to replace the cached predicate with
                                    // this new predicate?

                                    // Replace the current watcher with the new predicate watcher.
                                    nogood.swap(1, i);
                                    pumpkin_assert_moderate!(
                                        nogood[i].get_domain() == update_info.0
                                    );
                                    self.potential_empty_lists.push(nogood[i].get_domain());
                                    // Add this nogood to the watch list of the new watcher.
                                    // Ensure there is an entry.
                                    #[allow(clippy::map_entry)]
                                    if !self.watch_lists.contains_key(&nogood[1].get_domain()) {
                                        let _ = self
                                            .watch_lists
                                            .insert(nogood[1].get_domain(), WatchList::default());
                                    }
                                    match nogood[1] {
                                        IntegerPredicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .lower_bound
                                            .push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .upper_bound
                                            .push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            }),
                                        IntegerPredicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => {
                                            self.watch_lists.get_mut(&domain_id).unwrap().hole.push(
                                                Watcher {
                                                    right_hand_side: not_equal_constant,
                                                    nogood_id,
                                                },
                                            )
                                        }
                                        IntegerPredicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self
                                            .watch_lists
                                            .get_mut(&domain_id)
                                            .unwrap()
                                            .equals
                                            .push(Watcher {
                                                right_hand_side: equality_constant,
                                                nogood_id,
                                            }),
                                    }

                                    // No propagation is taking place, go to the next nogood.
                                    break;
                                }
                            } // end iterating through the nogood

                            if found_new_watch {
                                // Note this nogood is effectively removed from the watch list
                                // of the the current predicate, since we
                                // are only incrementing the current index, and not copying
                                // anything to the end_index.

                                current_index += 1;
                                continue;
                            }

                            println!("\tok going in");

                            // Keep the current watch for this predicate.
                            self.watch_lists.get_mut(&update_info.0).unwrap().equals[end_index] =
                                self.watch_lists.get_mut(&update_info.0).unwrap().equals
                                    [current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason: PropositionalConjunction =
                                nogood[1..].iter().copied().collect();
                            pumpkin_assert_moderate!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));
                            println!("propagating {}", !nogood[0]);
                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                println!("erroni!");
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists.get_mut(&update_info.0).unwrap().equals
                                        [end_index] =
                                        self.watch_lists.get_mut(&update_info.0).unwrap().equals
                                            [current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists
                                    .get_mut(&update_info.0)
                                    .unwrap()
                                    .equals
                                    .truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists
                            .get_mut(&update_info.0)
                            .unwrap()
                            .equals
                            .truncate(end_index);
                    }
                }
            }
        }
        // Handling a corner case that appears in the tests.
        // Normally there is a dummy entry at position 0 so there is something.
        self.last_index_on_trail = if !context.assignments.trail.is_empty() {
            context.assignments.trail.len() - 1
        } else {
            0
        };

        let _ = self.enqueued_updates.drain();
        for domain_id in self.potential_empty_lists.clone() {
            self.clean_up_entry_in_watch_list_if_needed(domain_id);
        }
        self.potential_empty_lists.clear();

        println!("after propagate");
        self.debug_print(context.assignments());

        Ok(())
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        self.last_index_on_trail = context.assignments().trail.len();
        let _ = self.enqueued_updates.drain();
    }

    fn notify(
        &mut self,
        context: &mut PropagationContextMut,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        // pumpkin_assert_moderate!(!self.is_update_present(local_id, event));
        // let update = (
        // DomainId {
        // id: local_id.unpack(),
        // },
        // event.unwrap(),
        // );

        let domain_id = DomainId {
            id: local_id.unpack(),
        };
        match event.unwrap() {
            IntDomainEvent::Assign => println!(
                "\te {} = {} {} {}",
                domain_id,
                context.assignments.get_lower_bound(domain_id),
                context.assignments.get_upper_bound(domain_id),
                context.assignments.is_domain_assigned(domain_id)
            ),
            IntDomainEvent::LowerBound => println!(
                "\te {} >= {} {} {}",
                domain_id,
                context.assignments.get_lower_bound(domain_id),
                context.assignments.get_upper_bound(domain_id),
                context.assignments.is_domain_assigned(domain_id)
            ),
            IntDomainEvent::UpperBound => println!(
                "\te {} <= {} {} {}",
                domain_id,
                context.assignments.get_lower_bound(domain_id),
                context.assignments.get_upper_bound(domain_id),
                context.assignments.is_domain_assigned(domain_id)
            ),
            IntDomainEvent::Removal => println!(
                "\te {} != {} {} {}",
                domain_id,
                context.assignments.get_lower_bound(domain_id),
                context.assignments.get_upper_bound(domain_id),
                context.assignments.is_domain_assigned(domain_id)
            ),
        }

        while local_id.unpack() as usize >= self.enqueued_updates.num_domains() {
            self.enqueued_updates.grow();
        }

        // Save the update,
        // and also enqueue removal in case the lower or upper bound updates are set.
        self.enqueued_updates.event_occurred(
            event.unwrap(),
            DomainId {
                id: local_id.unpack(),
            },
        );
        match event.unwrap() {
            IntDomainEvent::Assign => {
                // do nothing
            }
            IntDomainEvent::LowerBound => {
                self.enqueued_updates.event_occurred(
                    IntDomainEvent::Removal,
                    DomainId {
                        id: local_id.unpack(),
                    },
                );
            }
            IntDomainEvent::UpperBound => {
                self.enqueued_updates.event_occurred(
                    IntDomainEvent::Removal,
                    DomainId {
                        id: local_id.unpack(),
                    },
                );
            }
            IntDomainEvent::Removal => {
                // do nothing
            }
        }
        EnqueueDecision::Enqueue
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
    fn hack_add_asserting_nogood(
        &mut self,
        nogood: Vec<IntegerPredicate>,
        context: &mut PropagationContextMut,
    ) {
        if nogood.len() == 1 {
            self.add_permanent_nogood(nogood, context)
                .expect("Unit learned nogoods cannot fail.");
            return;
        }

        self.debug_propagate_nogood_from_scratch(&nogood, context)
            .expect("Do not expect to fail propagating learned nogood.");

        let nogood_id = NogoodId {
            id: self.nogoods.len() as u32,
        };
        self.add_watcher(nogood[0], nogood_id);
        self.add_watcher(nogood[1], nogood_id);
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

/// The watch list is specific to a domain id.
#[derive(Default, Clone, Debug)]
struct WatchList {
    /// Nogoods with a watched predicate [x >= k]
    lower_bound: Vec<Watcher>,
    /// Nogoods with a watched predicate [x <= k]
    upper_bound: Vec<Watcher>,
    /// Nogoods with a watched predicate [x != k]
    hole: Vec<Watcher>,
    /// Nogoods with a watched predicate [x == k]
    equals: Vec<Watcher>,
}

/// The watcher is with respect to a specific domain id and predicate type.
#[derive(Default, Clone, Copy, Debug)]
struct Watcher {
    // This field represents the right-hand side of the predicate present in the nogood.
    // It is used as an indicator to whether the nogood should be inspected.
    right_hand_side: i32,
    nogood_id: NogoodId,
    // todo: consider the cached literal
}

// impl Display for Watcher {
// fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// let rhs = self.right_hand_side;
// let ngid = self.nogood_id.id;
// write!(f, "[{rhs}: {ngid}]")
// }
// }

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

    use crate::conjunction;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::propagators::NogoodPropagatorConstructor;

    #[test]
    fn ternary_nogood_propagate() {
        let mut solver = TestSolver::default();
        let dummy = solver.new_variable(0, 1);
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let mut propagator = solver
            .new_propagator(NogoodPropagatorConstructor {})
            .expect("no empty domains");

        let _ = solver.increase_lower_bound_and_notify(&mut propagator, dummy.id as i32, dummy, 1);

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let context = &mut solver.get_propagation_context_mut(PropagatorId(0));
            propagator
                .hack_add_nogood(nogood.into(), context)
                .expect("");
        }

        let _ = solver.increase_lower_bound_and_notify(&mut propagator, a.id as i32, a, 3);
        let _ = solver.increase_lower_bound_and_notify(&mut propagator, b.id as i32, b, 0);

        solver
            .propagate_until_fixed_point(&mut propagator)
            .expect("");

        let _ = solver.increase_lower_bound_and_notify(&mut propagator, c.id as i32, c, 15);

        solver.propagate(&mut propagator).expect("");

        assert_eq!(solver.upper_bound(b), 0);

        let reason_lb = solver.get_reason_int(predicate!(b <= 0));
        assert_eq!(conjunction!([a >= 2] & [c >= 10]), *reason_lb);
    }

    #[test]
    fn unsat() {
        let mut solver = TestSolver::default();
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let mut propagator = solver
            .new_propagator(NogoodPropagatorConstructor {})
            .expect("no empty domains");

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let context = &mut solver.get_propagation_context_mut(PropagatorId(0));
            propagator
                .hack_add_nogood(nogood.into(), context)
                .expect("");
        }

        let _ = solver.increase_lower_bound_and_notify(&mut propagator, a.id as i32, a, 3);
        let _ = solver.increase_lower_bound_and_notify(&mut propagator, b.id as i32, b, 1);
        let _ = solver.increase_lower_bound_and_notify(&mut propagator, c.id as i32, c, 15);

        let result = solver.propagate_until_fixed_point(&mut propagator);
        assert!(result.is_err());
    }
}
