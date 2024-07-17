use std::cell::RefCell;
use std::ops::Not;
use std::rc::Rc;

use log::warn;

use crate::basic_types::ConstraintOperationError;
use crate::basic_types::HashSet;
use crate::basic_types::Inconsistency;
use crate::basic_types::KeyedVec;
use crate::basic_types::PropositionalConjunction;
use crate::basic_types::StorageKey;
use crate::conjunction;
use crate::engine::conflict_analysis::AdvancedNogood;
use crate::engine::opaque_domain_event::OpaqueDomainEvent;
use crate::engine::predicates::predicate::Predicate;
use crate::engine::propagation::propagation_context::HasAssignments;
use crate::engine::propagation::EnqueueDecision;
use crate::engine::propagation::LocalId;
use crate::engine::propagation::PropagationContext;
use crate::engine::propagation::PropagationContextMut;
use crate::engine::propagation::Propagator;
use crate::engine::propagation::PropagatorConstructor;
use crate::engine::propagation::PropagatorConstructorContext;
use crate::engine::propagation::ReadDomains;
use crate::engine::reason::LazyReason;
use crate::engine::variables::DomainId;
use crate::engine::Assignments;
use crate::engine::EventSink;
use crate::engine::IntDomainEvent;
use crate::predicate;
use crate::pumpkin_assert_advanced;
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
pub(crate) struct NogoodPropagatorConstructor;

impl PropagatorConstructor for NogoodPropagatorConstructor {
    type Propagator = NogoodPropagator;

    fn create(self, mut _context: &mut PropagatorConstructorContext<'_>) -> Self::Propagator {
        NogoodPropagator::default()
    }
}

#[derive(Default, Clone, Debug)]
struct Nogood {
    predicates: Vec<Predicate>,
    #[allow(dead_code)]
    is_learned: bool,
}

#[derive(Clone, Copy, Debug, Hash)]
#[allow(dead_code)]
struct PairDomainEvent {
    domain_id: DomainId,
    event: IntDomainEvent,
}

#[derive(Default, Clone, Debug)]
pub(crate) struct NogoodPropagator {
    nogoods: KeyedVec<NogoodId, Rc<RefCell<Nogood>>>,
    permanent_nogoods: Vec<NogoodId>,
    learned_nogoods: Vec<NogoodId>,
    // The trail index is used to determine the domains of the variables since last time.
    last_index_on_trail: usize,
    is_in_infeasible_state: bool,
    // Watch lists for the nogood propagator.
    // todo: could improve the data structure for watching.
    // todo: no point in using HashMaps, could us a simple array with direct hashing.
    watch_lists: KeyedVec<DomainId, WatchList>,
    enqueued_updates: EventSink, /* HashSet<(DomainId, IntDomainEvent)>, // Vec<(DomainId,
                                  * IntDomainEvent)>, */
}

impl NogoodPropagator {
    /// Does simple preprocessing, modifying the input nogood by:
    ///     1. Removing duplicate predicates.
    ///     2. Removing satisfied predicates at the root.
    ///     3. Detecting predicates falsified at the root. In that case, the nogood is preprocessed
    ///        to the empty nogood.
    ///     4. Conflicting predicates?
    fn preprocess_nogood(nogood: &mut Vec<Predicate>, context: &mut PropagationContextMut) {
        pumpkin_assert_simple!(context.get_decision_level() == 0);
        // The code below is broken down into several parts,
        // could be done more efficiently but probably okay.

        // println!("In comes the nogood: {:?}", nogood);

        // Check if the nogood cannot be violated, i.e., it has a falsified predicate.
        if nogood.is_empty() || nogood.iter().any(|p| context.is_predicate_falsified(*p)) {
            *nogood = vec![Predicate::trivially_false()];
            return;
        }

        // Remove predicates that are satisfied at the root level.
        nogood.retain(|p| !context.is_predicate_satisfied(*p));

        // println!("after removing sat predicates : {:?}", nogood);

        // If the nogood is violating at the root, the previous retain would leave an empty
        // Return a violating nogood.
        if nogood.is_empty() {
            *nogood = vec![Predicate::trivially_true()];
            return;
        }

        // We now remove duplicated predicates.
        let mut present_predicates: HashSet<Predicate> = HashSet::default();
        // We make use that adding elements to a hashset returns true if the element was not present
        // in the set.
        nogood.retain(|p| present_predicates.insert(*p));

        // println!("after removing duplicates: {:?}", nogood);

        // Check for contradicting predicates. In this case, the nogood cannot lead to propagation,
        // so we can ignore it.
        // Todo: The current version only partially does this, since there may be symmetries that
        // are not detected, e.g., for a 0-1 integer variable, [x >= 1] and [x == 0] are
        // opposite predicates but we do not detect these, and only check for direct
        // negatives [x <= 0] and [x == 1], [x == 1] & [x == 2] is not sensible.
        if nogood.iter().any(|p| present_predicates.contains(&p.not())) {
            *nogood = vec![Predicate::trivially_false()];
        }

        // This is a way to do semantic minimisation.
        let mut temp = AdvancedNogood::new(0);
        temp.add_predicates(nogood.clone(), context.assignments, None);
        *nogood = temp.extract_final_learned_nogood(context.assignments());
        // println!("after extraction: {:?}", nogood);
    }

    // Learned nogood during search.
    pub(crate) fn add_asserting_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
    ) {
        // println!("Learn: {:?}", nogood);

        if nogood.len() == 1 {
            self.add_permanent_nogood(nogood, context)
                .expect("Unit learned nogoods cannot fail.");
            return;
        }

        let nogood_id = NogoodId {
            id: self.nogoods.len() as u32,
        };
        self.add_watcher(nogood[0], nogood_id);
        self.add_watcher(nogood[1], nogood_id);
        self.nogoods.push(Rc::new(RefCell::new(Nogood {
            predicates: nogood,
            is_learned: true,
        })));

        self.debug_propagate_nogood_from_scratch(nogood_id, context)
            .expect("Do not expect to fail propagating learned nogood.");

        self.learned_nogoods.push(nogood_id);

        // TODO PROPAGATION
        // todo!();
    }

    pub(crate) fn add_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
    ) -> Result<(), ConstraintOperationError> {
        // println!("Perma: {:?}", nogood);

        match self.add_permanent_nogood(nogood, context) {
            Ok(_) => Ok(()),
            Err(e) => {
                self.is_in_infeasible_state = true;
                Err(e)
            }
        }
    }

    #[allow(dead_code)]
    fn add_permanent_nogood(
        &mut self,
        mut nogood: Vec<Predicate>,
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

        // println!("before process: {:?}", nogood);

        Self::preprocess_nogood(&mut nogood, context);

        // println!("after process: {:?}", nogood);

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

            self.nogoods.push(Rc::new(RefCell::new(Nogood {
                predicates: nogood,
                is_learned: false,
            })));

            Ok(())
        }
    }

    fn add_watcher(&mut self, predicate: Predicate, nogood_id: NogoodId) {
        // Add this nogood to the watch list of the new watcher.

        if predicate.get_domain().id as usize >= self.watch_lists.len() {
            self.watch_lists.resize(
                (predicate.get_domain().id + 1) as usize,
                WatchList::default(),
            );
        }

        match predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => self.watch_lists[domain_id].lower_bound.push(Watcher {
                right_hand_side: lower_bound,
                nogood_id,
            }),
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => self.watch_lists[domain_id].upper_bound.push(Watcher {
                right_hand_side: upper_bound,
                nogood_id,
            }),
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => self.watch_lists[domain_id].hole.push(Watcher {
                right_hand_side: not_equal_constant,
                nogood_id,
            }),
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => self.watch_lists[domain_id].equals.push(Watcher {
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
        nogood_id: NogoodId,
        context: &mut PropagationContextMut,
    ) -> Result<(), Inconsistency> {
        // Inefficient way of propagating, but okay for testing purposes
        // Explicitly goes through each predicate, and does multiple passes.

        let nogood = &self.nogoods[nogood_id];

        let num_falsified_predicates = nogood
            .borrow()
            .predicates
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| !x))
            .count();

        // if at least one predicate is false, then the nogood can be skipped
        if num_falsified_predicates > 0 {
            return Ok(());
        }

        let num_satisfied_predicates = nogood
            .borrow()
            .predicates
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| x))
            .count();

        let nogood_len = nogood.borrow().predicates.len();
        assert!(num_satisfied_predicates + num_falsified_predicates <= nogood_len);

        // If all predicates in the nogood are satisfied, there is a conflict.
        if num_satisfied_predicates == nogood_len {
            return Err(Inconsistency::Conflict {
                conflict_nogood: nogood.borrow().predicates.iter().copied().collect(),
            });
        }
        // If all but one predicate are satisfied, then we can propagate.
        // Note that this only makes sense since we know that there are no falsifying predicates at
        // this point.
        else if num_satisfied_predicates == nogood_len - 1 {
            // Note that we negate the remaining unassigned predicate!
            let propagated_predicate = nogood
                .borrow()
                .predicates
                .iter()
                .find(|predicate| context.evaluate_predicate(**predicate).is_none())
                .unwrap()
                .not();

            assert!(nogood
                .borrow()
                .predicates
                .iter()
                .any(|p| *p == propagated_predicate.not()));

            // Cannot use lazy explanations when propagating from scratch,
            // since the propagated predicate may not be at position zero,
            // but we cannot change the nogood since this function is with nonmutable self.
            // So we need to do eager then.
            // let reason = NogoodReason {
            //    nogood: Rc::clone(nogood),
            //};

            let reason: PropositionalConjunction = nogood
                .borrow()
                .predicates
                .iter()
                .filter(|p| **p != !propagated_predicate)
                .copied()
                .collect();

            // println!("from scratching: {}", propagated_predicate);
            // println!("...because of: {:?}", reason);

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

    // fn add_watcher(&mut self, predicate: Predicate, nogood_id: NogoodId) {
    // todo: look up Hashset operations to do this properly.
    // Create an entry in case the predicate is missing
    // if !self.watch_lists.contains_key(&predicate.get_domain()) {
    // let _ = self
    // .watch_lists
    // .insert(predicate.get_domain(), WatchList::default());
    // }
    //
    // match predicate {
    // Predicate::LowerBound {
    // domain_id,
    // lower_bound,
    // } => todo!(),
    // Predicate::UpperBound {
    // domain_id,
    // upper_bound,
    // } => todo!(),
    // Predicate::NotEqual {
    // domain_id,
    // not_equal_constant,
    // } => todo!(),
    // Predicate::Equal {
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

            for p in nogood.borrow().predicates.iter() {
                let m = match assignments.evaluate_predicate(*p) {
                    Some(b) => b as i32,
                    None => -1,
                };

                print!("{} ({}) ", p, m);
            }
            println!();
        }

        println!("= list");
        for m in self.watch_lists.iter().enumerate() {
            println!("var x{}: {:?}", m.0, m.1.equals);
        }

        println!("+++++++");
    }

    fn debug_is_properly_watched(&self) -> bool {
        let is_watching =
            |predicate: Predicate, nogood_id: NogoodId| -> bool {
                match predicate {
                    Predicate::LowerBound {
                        domain_id,
                        lower_bound,
                    } => self.watch_lists[domain_id].lower_bound.iter().any(|w| {
                        w.right_hand_side == lower_bound && w.nogood_id.id == nogood_id.id
                    }),
                    Predicate::UpperBound {
                        domain_id,
                        upper_bound,
                    } => self.watch_lists[domain_id].upper_bound.iter().any(|w| {
                        w.right_hand_side == upper_bound && w.nogood_id.id == nogood_id.id
                    }),
                    Predicate::NotEqual {
                        domain_id,
                        not_equal_constant,
                    } => self.watch_lists[domain_id].hole.iter().any(|w| {
                        w.right_hand_side == not_equal_constant && w.nogood_id.id == nogood_id.id
                    }),
                    Predicate::Equal {
                        domain_id,
                        equality_constant,
                    } => self.watch_lists[domain_id].equals.iter().any(|w| {
                        w.right_hand_side == equality_constant && w.nogood_id.id == nogood_id.id
                    }),
                }
            };

        for nogood in self.nogoods.iter().enumerate() {
            let nogood_id = NogoodId {
                id: nogood.0 as u32,
            };
            assert!(
                is_watching(nogood.1.borrow().predicates[0], nogood_id)
                    && is_watching(nogood.1.borrow().predicates[1], nogood_id)
            );
        }
        true
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

    fn propagate(&mut self, mut context: PropagationContextMut) -> Result<(), Inconsistency> {
        // old version from scratch:
        // let result = self.debug_propagate_from_scratch(context);
        // self.last_index_on_trail = context.assignments.trail.len() - 1;
        // return result;

        pumpkin_assert_advanced!(self.debug_is_properly_watched());

        if self.watch_lists.len() <= context.assignments().num_domains() as usize {
            self.watch_lists.resize(
                context.assignments().num_domains() as usize + 1,
                WatchList::default(),
            );
        }

        let old_trail_position = context.assignments.trail.len() - 1;

        // for t in context.assignments.trail.iter() {
        // println!("\t{}, dec: {}", t.predicate, t.reason.is_none());
        // }
        //
        // println!("before propagate");
        // self.debug_print(context.assignments());

        // Because drain lazily removes and updates internal data structures, in case a conflict is
        // detected and the loop exists, some elements might not get cleaned up properly. So we
        // eager call each elements here by copying. Could think about avoiding this in the future.
        let events: Vec<(IntDomainEvent, DomainId)> = self.enqueued_updates.drain().collect();

        // println!("Before prop num events: {}", events.len());
        // println!("elems: {:?}", events);
        //
        // for var in events.iter() {
        // println!(
        // "var {}: lb = {}, ub = {}",
        // var.1,
        // context.assignments.get_lower_bound(var.1),
        // context.assignments.get_upper_bound(var.1),
        // );
        // }

        for update_info in events {
            let update_info: (DomainId, IntDomainEvent) = (update_info.1, update_info.0);
            // println!("\tmm: {}", update_info.0);
            // for m in self.watch_lists[update_info.0].hole.iter() {
            // println!("w {}: {:?}", m.right_hand_side, self.nogoods[m.nogood_id]);
            // }
            //
            // match update_info.1 {
            // IntDomainEvent::Assign => println!("ass"),
            // IntDomainEvent::LowerBound => println!("lb"),
            // IntDomainEvent::UpperBound => println!("ub"),
            // IntDomainEvent::Removal => println!("rem"),
            // }

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
                    let num_watchers = self.watch_lists[update_info.0].lower_bound.len();
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side = self.watch_lists[update_info.0].lower_bound
                            [current_index]
                            .right_hand_side;

                        if old_lower_bound < right_hand_side && right_hand_side <= new_lower_bound {
                            // todo: check cached predicate?

                            let nogood_id = self.watch_lists[update_info.0].lower_bound
                                [current_index]
                                .nogood_id;

                            let nogood = &mut self.nogoods[nogood_id].borrow_mut().predicates;

                            let is_watched_predicate = |predicate: Predicate| {
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
                                self.watch_lists[update_info.0].lower_bound[end_index] =
                                    self.watch_lists[update_info.0].lower_bound[current_index];
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
                                    // Add this nogood to the watch list of the new watcher.
                                    match nogood[1] {
                                        Predicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => {
                                            self.watch_lists[domain_id].lower_bound.push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => {
                                            self.watch_lists[domain_id].upper_bound.push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => self.watch_lists[domain_id].hole.push(Watcher {
                                            right_hand_side: not_equal_constant,
                                            nogood_id,
                                        }),
                                        Predicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self.watch_lists[domain_id].equals.push(Watcher {
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
                            self.watch_lists[update_info.0].lower_bound[end_index] =
                                self.watch_lists[update_info.0].lower_bound[current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = NogoodReason {
                                nogood: Rc::clone(&self.nogoods[nogood_id]),
                            };

                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[update_info.0].lower_bound[end_index] =
                                        self.watch_lists[update_info.0].lower_bound[current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists[update_info.0]
                                    .lower_bound
                                    .truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[update_info.0].lower_bound[end_index] =
                                self.watch_lists[update_info.0].lower_bound[current_index];
                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[update_info.0]
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
                    let num_watchers = self.watch_lists[update_info.0].upper_bound.len();
                    // println!("num watching {}", num_watchers);
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side = self.watch_lists[update_info.0].upper_bound
                            [current_index]
                            .right_hand_side;

                        // println!("...hmm {old_upper_bound} > {right_hand_side} &&
                        // {right_hand_side} >= {new_upper_bound}");
                        // println!("trail pos: {}", self.last_index_on_trail);
                        // println!(
                        // "how {:?}",
                        // context.assignments.domains[update_info.0].upper_bound_updates
                        // );

                        if old_upper_bound > right_hand_side && right_hand_side >= new_upper_bound {
                            // println!("Im in?");
                            // todo: check cached predicate?

                            let nogood_id = self.watch_lists[update_info.0].upper_bound
                                [current_index]
                                .nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].borrow_mut().predicates;

                            let is_watched_predicate = |predicate: Predicate| {
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
                                self.watch_lists[update_info.0].upper_bound[end_index] =
                                    self.watch_lists[update_info.0].upper_bound[current_index];
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
                                    // Add this nogood to the watch list of the new watcher.
                                    match nogood[1] {
                                        Predicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => {
                                            self.watch_lists[domain_id].lower_bound.push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => {
                                            self.watch_lists[domain_id].upper_bound.push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => self.watch_lists[domain_id].hole.push(Watcher {
                                            right_hand_side: not_equal_constant,
                                            nogood_id,
                                        }),
                                        Predicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self.watch_lists[domain_id].equals.push(Watcher {
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
                            self.watch_lists[update_info.0].upper_bound[end_index] =
                                self.watch_lists[update_info.0].upper_bound[current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = NogoodReason {
                                nogood: Rc::clone(&self.nogoods[nogood_id]),
                            };

                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[update_info.0].upper_bound[end_index] =
                                        self.watch_lists[update_info.0].upper_bound[current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists[update_info.0]
                                    .upper_bound
                                    .truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[update_info.0].upper_bound[end_index] =
                                self.watch_lists[update_info.0].upper_bound[current_index];
                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[update_info.0]
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
                    let num_watchers = self.watch_lists[update_info.0].hole.len();
                    // Iterate through all watchers.
                    while current_index < num_watchers {
                        let right_hand_side =
                            self.watch_lists[update_info.0].hole[current_index].right_hand_side;

                        let update_domain = update_info.0;
                        // Only look at the watcher if:
                        // 1) The removed value was definitely removed due to bound changes, OR
                        // 2) The removed value is within the bounds, and was actually removed.
                        if old_upper_bound >= right_hand_side && right_hand_side > new_upper_bound
                            || old_lower_bound <= right_hand_side
                                && right_hand_side < new_lower_bound
                            || (new_lower_bound < right_hand_side
                                && right_hand_side < new_upper_bound
                                && context.is_predicate_satisfied(predicate!(
                                    update_domain != right_hand_side
                                )))
                        {
                            // todo: check cached predicate?

                            let nogood_id =
                                self.watch_lists[update_info.0].hole[current_index].nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].borrow_mut().predicates;

                            let is_watched_predicate = |predicate: Predicate| {
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
                                self.watch_lists[update_info.0].hole[end_index] =
                                    self.watch_lists[update_info.0].hole[current_index];
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
                                    // Add this nogood to the watch list of the new watcher.
                                    match nogood[1] {
                                        Predicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => {
                                            self.watch_lists[domain_id].lower_bound.push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => {
                                            self.watch_lists[domain_id].upper_bound.push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => self.watch_lists[domain_id].hole.push(Watcher {
                                            right_hand_side: not_equal_constant,
                                            nogood_id,
                                        }),
                                        Predicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self.watch_lists[domain_id].equals.push(Watcher {
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
                            self.watch_lists[update_info.0].hole[end_index] =
                                self.watch_lists[update_info.0].hole[current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = NogoodReason {
                                nogood: Rc::clone(&self.nogoods[nogood_id]),
                            };

                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[update_info.0].hole[end_index] =
                                        self.watch_lists[update_info.0].hole[current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists[update_info.0].hole.truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[update_info.0].hole[end_index] =
                                self.watch_lists[update_info.0].hole[current_index];
                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[update_info.0].hole.truncate(end_index);
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
                    let num_watchers = self.watch_lists[update_info.0].equals.len();
                    // Iterate through all watchers.

                    while current_index < num_watchers {
                        let right_hand_side =
                            self.watch_lists[update_info.0].equals[current_index].right_hand_side;

                        // println!("\teq {} = {}", update_info.0, assigned_value);
                        // println!("\tassn val: {}", assigned_value);
                        // println!("\t rhs: {}", right_hand_side);

                        if assigned_value == right_hand_side {
                            // todo: check cached predicate?

                            let nogood_id =
                                self.watch_lists[update_info.0].equals[current_index].nogood_id;
                            let nogood = &mut self.nogoods[nogood_id].borrow_mut().predicates;

                            let is_watched_predicate = |predicate: Predicate| {
                                predicate.is_equality_predicate()
                                    && predicate.get_domain() == update_info.0
                                    && predicate.get_right_hand_side() == assigned_value
                            };

                            // Place the watched predicate at position 1 for simplicity.
                            if is_watched_predicate(nogood[0]) {
                                nogood.swap(0, 1);
                            }

                            // println!("eye: {:?}", nogood);

                            pumpkin_assert_moderate!(context.is_predicate_satisfied(nogood[1]));

                            // Check the other watched predicate is already falsified, in which case
                            // no propagation can take place. Recall that the other watched
                            // predicate is at position 0 due to previous code.
                            // todo: check if comparing to the cache literal would make sense.
                            if context.is_predicate_falsified(nogood[0]) {
                                // println!("!!falsi");
                                // Keep the watchers, the nogood is falsified,
                                // no propagation can take place.
                                self.watch_lists[update_info.0].equals[end_index] =
                                    self.watch_lists[update_info.0].equals[current_index];
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
                                    // Add this nogood to the watch list of the new watcher.
                                    // Ensure there is an entry.
                                    match nogood[1] {
                                        Predicate::LowerBound {
                                            domain_id,
                                            lower_bound,
                                        } => {
                                            self.watch_lists[domain_id].lower_bound.push(Watcher {
                                                right_hand_side: lower_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::UpperBound {
                                            domain_id,
                                            upper_bound,
                                        } => {
                                            self.watch_lists[domain_id].upper_bound.push(Watcher {
                                                right_hand_side: upper_bound,
                                                nogood_id,
                                            })
                                        }
                                        Predicate::NotEqual {
                                            domain_id,
                                            not_equal_constant,
                                        } => self.watch_lists[domain_id].hole.push(Watcher {
                                            right_hand_side: not_equal_constant,
                                            nogood_id,
                                        }),
                                        Predicate::Equal {
                                            domain_id,
                                            equality_constant,
                                        } => self.watch_lists[domain_id].equals.push(Watcher {
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

                            // println!("\tok going in");

                            // Keep the current watch for this predicate.
                            self.watch_lists[update_info.0].equals[end_index] =
                                self.watch_lists[update_info.0].equals[current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood[1..]
                                .iter()
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = NogoodReason {
                                nogood: Rc::clone(&self.nogoods[nogood_id]),
                            };

                            // println!("propagating {}", !nogood[0]);
                            let result = context.post_predicate(!nogood[0], reason);
                            // If the propagation lead to a conflict.
                            if let Err(e) = result {
                                //  println!("erroni!");
                                // Stop any further propagation and report the conflict.
                                // Readd the remaining watchers to the watch list.
                                while current_index < num_watchers {
                                    self.watch_lists[update_info.0].equals[end_index] =
                                        self.watch_lists[update_info.0].equals[current_index];
                                    current_index += 1;
                                    end_index += 1;
                                }
                                self.watch_lists[update_info.0].equals.truncate(end_index);
                                return Err(e.into());
                            }
                        } else {
                            // Keep the current watch for this predicate.
                            self.watch_lists[update_info.0].equals[end_index] =
                                self.watch_lists[update_info.0].equals[current_index];
                            end_index += 1;
                            current_index += 1;
                        }
                    }
                    // Went through all the watchers.
                    if num_watchers > 0 {
                        self.watch_lists[update_info.0].equals.truncate(end_index);
                    }
                }
            }
        }
        self.last_index_on_trail = old_trail_position;

        // let _ = self.enqueued_updates.drain();

        // println!("after propagate");
        // self.debug_print(context.assignments());

        pumpkin_assert_advanced!(self.debug_is_properly_watched());

        Ok(())
    }

    fn synchronise(&mut self, context: &PropagationContext) {
        self.last_index_on_trail = context.assignments().trail.len() - 1;
        let _ = self.enqueued_updates.drain();
    }

    fn notify(
        &mut self,
        _context: PropagationContext,
        local_id: LocalId,
        event: OpaqueDomainEvent,
    ) -> EnqueueDecision {
        // let domain_id = DomainId {
        // id: local_id.unpack(),
        // };
        // match event.unwrap() {
        // IntDomainEvent::Assign => println!(
        // "\te {} = {} {} {}",
        // domain_id,
        // context.assignments.get_lower_bound(domain_id),
        // context.assignments.get_upper_bound(domain_id),
        // context.assignments.is_domain_assigned(domain_id)
        // ),
        // IntDomainEvent::LowerBound => println!(
        // "\te {} >= {} {} {}",
        // domain_id,
        // context.assignments.get_lower_bound(domain_id),
        // context.assignments.get_upper_bound(domain_id),
        // context.assignments.is_domain_assigned(domain_id)
        // ),
        // IntDomainEvent::UpperBound => println!(
        // "\te {} <= {} {} {}",
        // domain_id,
        // context.assignments.get_lower_bound(domain_id),
        // context.assignments.get_upper_bound(domain_id),
        // context.assignments.is_domain_assigned(domain_id)
        // ),
        // IntDomainEvent::Removal => println!(
        // "\te {} != {} {} {}",
        // domain_id,
        // context.assignments.get_lower_bound(domain_id),
        // context.assignments.get_upper_bound(domain_id),
        // context.assignments.is_domain_assigned(domain_id)
        // ),
        // }

        while local_id.unpack() as usize >= self.enqueued_updates.num_domains() {
            self.enqueued_updates.grow();
        }

        // if local_id.unpack() as usize >= self.watch_lists.len() {
        // self.watch_lists
        // .resize((local_id.unpack() + 1) as usize, WatchList::default());
        // }

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
        mut context: PropagationContextMut,
    ) -> Result<(), Inconsistency> {
        // Very inefficient version!

        // The algorithm goes through every nogood explicitly
        // and computes from scratch.
        for nogood_id in self.nogoods.keys() {
            self.debug_propagate_nogood_from_scratch(nogood_id, &mut context)?;
        }
        Ok(())
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

struct NogoodReason {
    nogood: Rc<RefCell<Nogood>>,
}

impl LazyReason for NogoodReason {
    fn compute(self: Box<Self>, _: &PropagationContext) -> PropositionalConjunction {
        // We assume the propagated predicate is at position 0 in the nogood.
        self.nogood
            .borrow()
            .predicates
            .iter()
            .skip(1)
            .copied()
            .collect()
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

    use super::NogoodPropagator;
    use crate::conjunction;
    use crate::engine::propagation::Propagator;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::propagators::nogood::NogoodPropagatorConstructor;

    fn downcast_to_nogood_propagator(
        nogood_propagator: &mut Box<dyn Propagator>,
    ) -> &mut NogoodPropagator {
        match nogood_propagator.downcast_mut::<NogoodPropagator>() {
            Some(nogood_propagator) => nogood_propagator,
            None => panic!("Provided propagator should be the nogood propagator"),
        }
    }

    #[test]
    fn ternary_nogood_propagate() {
        let mut solver = TestSolver::default();
        let dummy = solver.new_variable(0, 1);
        let a = solver.new_variable(1, 3);
        let b = solver.new_variable(-4, 4);
        let c = solver.new_variable(-10, 20);

        let mut propagator = solver
            .new_propagator(NogoodPropagatorConstructor)
            .expect("no empty domains");

        let _ = solver.increase_lower_bound_and_notify(&mut propagator, dummy.id as i32, dummy, 1);

        let nogood = conjunction!([a >= 2] & [b >= 1] & [c >= 10]);
        {
            let context = &mut solver.get_propagation_context_mut(PropagatorId(0));
            downcast_to_nogood_propagator(&mut propagator)
                .add_nogood(nogood.into(), context)
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
            downcast_to_nogood_propagator(&mut propagator)
                .add_nogood(nogood.into(), context)
                .expect("");
        }

        let _ = solver.increase_lower_bound_and_notify(&mut propagator, a.id as i32, a, 3);
        let _ = solver.increase_lower_bound_and_notify(&mut propagator, b.id as i32, b, 1);
        let _ = solver.increase_lower_bound_and_notify(&mut propagator, c.id as i32, c, 15);

        let result = solver.propagate_until_fixed_point(&mut propagator);
        assert!(result.is_err());
    }
}
