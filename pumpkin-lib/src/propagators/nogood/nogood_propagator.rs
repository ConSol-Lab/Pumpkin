use std::ops::Not;

use log::warn;

use crate::basic_types::ConstraintOperationError;
use crate::basic_types::Inconsistency;
use crate::basic_types::PropositionalConjunction;
use crate::conjunction;
use crate::containers::KeyedVec;
use crate::containers::StorageKey;
use crate::engine::nogoods::Lbd;
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
use crate::engine::reason::Reason;
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
    predicates: PropositionalConjunction,
    is_learned: bool,
    lbd: u32,
    is_protected: bool,
    is_deleted: bool,
    activity: f32,
}

impl Nogood {
    fn new_learned_nogood(predicates: PropositionalConjunction, lbd: u32) -> Self {
        Nogood {
            predicates,
            is_learned: true,
            lbd,
            activity: 0.0,
            is_deleted: false,
            is_protected: false,
        }
    }

    fn new_permanent_nogood(predicates: PropositionalConjunction) -> Self {
        Nogood {
            predicates,
            is_learned: false,
            lbd: 0,
            activity: 0.0,
            is_deleted: false,
            is_protected: false,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct NogoodPropagator {
    nogoods: KeyedVec<NogoodId, Nogood>,
    permanent_nogoods: Vec<NogoodId>,
    learned_nogood_ids: LearnedNogoodIds,
    delete_ids: Vec<NogoodId>,
    // The trail index is used to determine the domains of the variables since last time.
    last_index_on_trail: usize,
    is_in_infeasible_state: bool,
    // Watch lists for the nogood propagator.
    // todo: could improve the data structure for watching.
    watch_lists: KeyedVec<DomainId, WatchList>,
    enqueued_updates: EventSink,
    lbd_helper: Lbd,
    activity_bump_increment: f32,
    parameters: LearningOptions,
}

#[derive(Default, Debug, Clone)]
struct LearnedNogoodIds {
    low_lbd: Vec<NogoodId>,
    high_lbd: Vec<NogoodId>,
}

#[derive(Debug, Copy, Clone)]
struct LearningOptions {
    max_activity: f32,
    activity_decay_factor: f32,
    limit_num_high_lbd_nogoods: usize,
    lbd_threshold: u32,
    nogood_sorting_strategy: LearnedNogoodSortingStrategy,
}

impl Default for LearningOptions {
    fn default() -> Self {
        Self {
            max_activity: 1e20,
            activity_decay_factor: 0.99,
            limit_num_high_lbd_nogoods: 4000,
            nogood_sorting_strategy: LearnedNogoodSortingStrategy::Lbd,
            lbd_threshold: 5,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LearnedNogoodSortingStrategy {
    #[allow(dead_code)]
    Activity,
    Lbd,
}

impl std::fmt::Display for LearnedNogoodSortingStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LearnedNogoodSortingStrategy::Lbd => write!(f, "lbd"),
            LearnedNogoodSortingStrategy::Activity => write!(f, "activity"),
        }
    }
}

impl Default for NogoodPropagator {
    fn default() -> Self {
        Self {
            nogoods: Default::default(),
            permanent_nogoods: Default::default(),
            learned_nogood_ids: Default::default(),
            delete_ids: Default::default(),
            last_index_on_trail: Default::default(),
            is_in_infeasible_state: Default::default(),
            watch_lists: Default::default(),
            enqueued_updates: Default::default(),
            lbd_helper: Lbd::default(),
            parameters: LearningOptions::default(),
            activity_bump_increment: 1.0,
        }
    }
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

        // We opt for semantic minimisation upfront. This way we avoid the possibility of having
        // assigned predicates in the final nogood. This could happen since the root bound can
        // change since the initial time the semantic minimiser recorded it, so it would not know
        // that a previously nonroot bound is now actually a root bound.

        // Semantic minimisation will take care of removing duplicate predicates, conflicting
        // nogoods, and may result in few predicates since it removes redundancies.
        *nogood = context
            .semantic_minimiser
            .minimise(nogood, context.assignments);

        // Check if the nogood cannot be violated, i.e., it has a falsified predicate.
        if nogood.is_empty() || nogood.iter().any(|p| context.is_predicate_falsified(*p)) {
            *nogood = vec![Predicate::trivially_false()];
            return;
        }

        // Remove predicates that are satisfied at the root level.
        nogood.retain(|p| !context.is_predicate_satisfied(*p));

        // If the nogood is violating at the root, the previous retain would leave an empty nogood.
        // Return a violating nogood.
        if nogood.is_empty() {
            *nogood = vec![Predicate::trivially_true()];
        }
        // Done with preprocessing, the result is stored in the input nogood.
    }

    // Learned nogood during search.
    pub(crate) fn add_asserting_nogood(
        &mut self,
        nogood: Vec<Predicate>,
        context: &mut PropagationContextMut,
    ) {
        // println!("nogood: {:?}", nogood);

        if nogood.len() == 1 {
            self.add_permanent_nogood(nogood, context)
                .expect("Unit learned nogoods cannot fail.");
            return;
        }

        // Skip the zero-th predicate since it is unassigned,
        // but will be assigned at the level of the predicate at index one.
        let lbd = self
            .lbd_helper
            .compute_lbd(&nogood.as_slice()[1..], context.assignments());

        // Add the nogood to the database.
        // If there is an available nogood id, use it, otherwise allocate a fresh id.
        let new_id = if let Some(reused_id) = self.delete_ids.pop() {
            self.nogoods[reused_id] = Nogood::new_learned_nogood(nogood.into(), lbd);
            reused_id
        } else {
            let new_nogood_id = NogoodId {
                id: self.nogoods.len() as u32,
            };
            self.nogoods
                .push(Nogood::new_learned_nogood(nogood.into(), lbd));
            new_nogood_id
        };

        self.add_watcher(self.nogoods[new_id].predicates[0], new_id);
        self.add_watcher(self.nogoods[new_id].predicates[1], new_id);

        let reason = Reason::DynamicLazy {
            code: new_id.id as u64,
        };
        context
            .post_predicate(!self.nogoods[new_id].predicates[0], reason)
            .expect("Cannot fail to add the asserting predicate.");

        if lbd <= self.parameters.lbd_threshold {
            self.learned_nogood_ids.low_lbd.push(new_id);
        } else {
            self.learned_nogood_ids.high_lbd.push(new_id);
        }
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
            // Add the nogood to the database.
            // If there is an available nogood id, use it, otherwise allocate a fresh id.
            let new_id = if let Some(reused_id) = self.delete_ids.pop() {
                self.nogoods[reused_id] = Nogood::new_permanent_nogood(nogood.into());
                reused_id
            } else {
                let new_nogood_id = NogoodId {
                    id: self.nogoods.len() as u32,
                };
                self.nogoods
                    .push(Nogood::new_permanent_nogood(nogood.into()));
                new_nogood_id
            };

            self.permanent_nogoods.push(new_id);

            self.add_watcher(self.nogoods[new_id].predicates[0], new_id);
            self.add_watcher(self.nogoods[new_id].predicates[1], new_id);

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
            .predicates
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| !x))
            .count();

        // if at least one predicate is false, then the nogood can be skipped
        if num_falsified_predicates > 0 {
            return Ok(());
        }

        let num_satisfied_predicates = nogood
            .predicates
            .iter()
            .filter(|predicate| context.evaluate_predicate(**predicate).is_some_and(|x| x))
            .count();

        let nogood_len = nogood.predicates.len();
        assert!(num_satisfied_predicates + num_falsified_predicates <= nogood_len);

        // If all predicates in the nogood are satisfied, there is a conflict.
        if num_satisfied_predicates == nogood_len {
            return Err(Inconsistency::Conflict {
                conflict_nogood: nogood.predicates.iter().copied().collect(),
            });
        }
        // If all but one predicate are satisfied, then we can propagate.
        // Note that this only makes sense since we know that there are no falsifying predicates at
        // this point.
        else if num_satisfied_predicates == nogood_len - 1 {
            // Note that we negate the remaining unassigned predicate!
            let propagated_predicate = nogood
                .predicates
                .iter()
                .find(|predicate| context.evaluate_predicate(**predicate).is_none())
                .unwrap()
                .not();

            assert!(nogood
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
                    } => self.watch_lists[domain_id]
                        .lower_bound
                        .iter()
                        .any(|w| w.right_hand_side == lower_bound && w.nogood_id == nogood_id),
                    Predicate::UpperBound {
                        domain_id,
                        upper_bound,
                    } => self.watch_lists[domain_id]
                        .upper_bound
                        .iter()
                        .any(|w| w.right_hand_side == upper_bound && w.nogood_id == nogood_id),
                    Predicate::NotEqual {
                        domain_id,
                        not_equal_constant,
                    } => self.watch_lists[domain_id].hole.iter().any(|w| {
                        w.right_hand_side == not_equal_constant && w.nogood_id == nogood_id
                    }),
                    Predicate::Equal {
                        domain_id,
                        equality_constant,
                    } => self.watch_lists[domain_id].equals.iter().any(|w| {
                        w.right_hand_side == equality_constant && w.nogood_id == nogood_id
                    }),
                }
            };

        for nogood in self.nogoods.iter().enumerate() {
            let nogood_id = NogoodId {
                id: nogood.0 as u32,
            };

            if !(is_watching(nogood.1.predicates[0], nogood_id)
                && is_watching(nogood.1.predicates[1], nogood_id))
            {
                println!("Nogood id: {}", nogood_id.id);
                println!("Nogood: {:?}", nogood);
                println!(
                    "watching 0: {}",
                    is_watching(nogood.1.predicates[0], nogood_id)
                );
                println!(
                    "watching 1: {}",
                    is_watching(nogood.1.predicates[1], nogood_id)
                );
                println!(
                    "watch list 0: {:?}",
                    self.watch_lists[nogood.1.predicates[0].get_domain()]
                );
                println!(
                    "watch list 1: {:?}",
                    self.watch_lists[nogood.1.predicates[1].get_domain()]
                );
            }

            assert!(
                is_watching(nogood.1.predicates[0], nogood_id)
                    && is_watching(nogood.1.predicates[1], nogood_id)
            );
        }
        true
    }

    fn clean_up_learned_nogoods_if_needed(&mut self, context: &PropagationContext) {
        // Only remove learned nogoods if there are too many.
        if self.learned_nogood_ids.high_lbd.len() > self.parameters.limit_num_high_lbd_nogoods {
            // The procedure is divided into two parts (for simplicity of implementation).
            // 1. Promote nogoods that are in the high lbd group but got updated to a low lbd.
            // 2. Remove roughly half of the nogoods that have high lbd.
            self.promote_high_lbd_nogoods();
            self.remove_high_lbd_nogoods(context);
        }
    }

    fn promote_high_lbd_nogoods(&mut self) {
        self.learned_nogood_ids.high_lbd.retain(|id| {
            // If the LBD is still high, the nogood stays in the high LBD category.
            if self.nogoods[*id].lbd > self.parameters.lbd_threshold {
                true
            }
            // Otherwise the nogood is promoted to the low LBD group.
            else {
                self.learned_nogood_ids.low_lbd.push(*id);
                false
            }
        })
    }

    fn remove_nogood_from_watch_list(
        watch_lists: &mut KeyedVec<DomainId, WatchList>,
        watching_predicate: Predicate,
        id: NogoodId,
    ) {
        match watching_predicate {
            Predicate::LowerBound {
                domain_id,
                lower_bound,
            } => {
                let position = watch_lists[domain_id]
                    .lower_bound
                    .iter()
                    .position(|w| w.right_hand_side == lower_bound && w.nogood_id == id)
                    .expect("LB watcher must be present.");
                let _ = watch_lists[domain_id].lower_bound.swap_remove(position);
            }
            Predicate::UpperBound {
                domain_id,
                upper_bound,
            } => {
                let position = watch_lists[domain_id]
                    .upper_bound
                    .iter()
                    .position(|w| w.right_hand_side == upper_bound && w.nogood_id == id)
                    .expect("UB watcher must be present.");
                let _ = watch_lists[domain_id].upper_bound.swap_remove(position);
            }
            Predicate::NotEqual {
                domain_id,
                not_equal_constant,
            } => {
                let position = watch_lists[domain_id]
                    .hole
                    .iter()
                    .position(|w| w.right_hand_side == not_equal_constant && w.nogood_id == id)
                    .expect("NE watcher must be present.");
                let _ = watch_lists[domain_id].hole.swap_remove(position);
            }
            Predicate::Equal {
                domain_id,
                equality_constant,
            } => {
                let position = watch_lists[domain_id]
                    .equals
                    .iter()
                    .position(|w| w.right_hand_side == equality_constant && w.nogood_id == id)
                    .expect("Assignment watcher must be present.");
                let _ = watch_lists[domain_id].equals.swap_remove(position);
            }
        }
    }

    fn remove_high_lbd_nogoods(&mut self, context: &PropagationContext) {
        // Currently we only remove at the root level for simplicity, but we could consider
        // otherwise.
        assert!(context.get_decision_level() == 0);

        // Roughly half of the learned nogoods will be removed.

        self.sort_high_lbd_nogoods_by_quality_decreasing_order();

        // The removal is done in two phases.
        // 1) Nogoods are deleted but the ids are not removed from self.learned_nogoods_ids.
        // 2) The corresponding ids are removed from the self.learned_nogoods_ids.
        let mut num_clauses_to_remove =
            self.learned_nogood_ids.high_lbd.len() - self.parameters.limit_num_high_lbd_nogoods / 2;
        // Note the 'rev', since poor nogoods have priority for deletion.
        // The aim is to remove half of the nogoods, but less could be removed due to protection.
        for &id in self.learned_nogood_ids.high_lbd.iter().rev() {
            if num_clauses_to_remove == 0 {
                break;
            }

            // Protected clauses are skipped for one clean up iteration.
            if self.nogoods[id].is_protected {
                self.nogoods[id].is_protected = false;
                continue;
            }

            // Remove the nogood from the watch list.
            // todo: could be potentially done more efficiently,
            // although currently this is not a bottleneck.
            Self::remove_nogood_from_watch_list(
                &mut self.watch_lists,
                self.nogoods[id].predicates[0],
                id,
            );
            Self::remove_nogood_from_watch_list(
                &mut self.watch_lists,
                self.nogoods[id].predicates[1],
                id,
            );

            // Delete the nogood.
            // Note that the deleted nogood is still kept in the database but it will not be used
            // for propagation. A new nogood may take the place of a deleted nogood, this makes it
            // simpler, since other nogood ids remain unchanged.
            self.nogoods[id].is_deleted = true;
            self.delete_ids.push(id);

            num_clauses_to_remove -= 1;
        }

        self.learned_nogood_ids
            .high_lbd
            .retain(|&id| !self.nogoods[id].is_deleted);
    }

    fn sort_high_lbd_nogoods_by_quality_decreasing_order(&mut self) {
        // The ordering is such that the 'better' nogoods are in front.
        // Note that this is not the most efficient sorting comparison, but will do for now.
        self.learned_nogood_ids
            .high_lbd
            .sort_unstable_by(|&id1, &id2| {
                let nogood1 = &self.nogoods[id1];
                let nogood2 = &self.nogoods[id2];

                match self.parameters.nogood_sorting_strategy {
                    LearnedNogoodSortingStrategy::Activity => {
                        // Note that here we reverse nogood1 and nogood2,
                        // because a higher value for activity is better.
                        nogood2.activity.partial_cmp(&nogood1.activity).unwrap()
                    }
                    LearnedNogoodSortingStrategy::Lbd => {
                        if nogood1.lbd != nogood2.lbd {
                            // Recall that lower LBD is better.
                            nogood1.lbd.cmp(&nogood2.lbd)
                        } else {
                            // Note that here we reverse nogood1 and nogood2,
                            // because a higher value for activity is better.
                            nogood2.activity.partial_cmp(&nogood1.activity).unwrap()
                        }
                    }
                }
            });
    }

    pub(crate) fn decay_nogood_activities(&mut self) {
        self.activity_bump_increment /= self.parameters.activity_decay_factor;
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

        // if self.watch_lists.len() > 4 {
        // println!("Watch list x4: {:?}", self.watch_lists[DomainId::new(4)]);
        // }
        //
        // if self.nogoods.len() > 11 {
        // println!("nogood: {:?}", self.nogoods[NogoodId { id: 11 }]);
        // }
        //
        // println!("----");

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

                            let nogood = &mut self.nogoods[nogood_id].predicates;

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
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy {
                                code: nogood_id.id as u64,
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
                            let nogood = &mut self.nogoods[nogood_id].predicates;

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
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy {
                                code: nogood_id.id as u64,
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
                            let nogood = &mut self.nogoods[nogood_id].predicates;

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
                            // The watcher for holes has a special case. In case the watcher that is
                            // going to replace this one is 1) a predicate with the same
                            // domain_id and 2) is also a not equals predicate, then the watcher
                            // should not be moved from this list, but instead only its right hand
                            // side should be changed to reflect the new watcher. The variable
                            // 'kept_watcher_new_rhs' holds info about this new rhs if appropriate.
                            let mut kept_watcher_new_rhs: Option<i32> = None;
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
                                        } => {
                                            // If the watcher indeed needs to move from this watch
                                            // list to another list.
                                            if domain_id != update_info.0 {
                                                self.watch_lists[domain_id].hole.push(Watcher {
                                                    right_hand_side: not_equal_constant,
                                                    nogood_id,
                                                })
                                            } else {
                                                // The watcher should stay in this list, but change
                                                // its right hand side to reflect the new watching
                                                // predicate. Here we only note that the watcher
                                                // should stay, and later it actually gets copied.
                                                kept_watcher_new_rhs = Some(not_equal_constant);
                                            }
                                        }
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
                                if let Some(new_rhs) = kept_watcher_new_rhs {
                                    // Keep the current watch for this predicate,
                                    // and update its right hand side.
                                    self.watch_lists[update_info.0].hole[end_index] =
                                        self.watch_lists[update_info.0].hole[current_index];
                                    self.watch_lists[update_info.0].hole[end_index]
                                        .right_hand_side = new_rhs;

                                    // pumpkin_assert_moderate!(
                                    // self.nogoods[self.watch_lists[update_info.0].hole
                                    // [end_index]
                                    // .nogood_id]
                                    // .as_ref()
                                    //
                                    // .predicates[1]
                                    // .get_right_hand_side()
                                    // == new_rhs
                                    // );

                                    end_index += 1;
                                    current_index += 1;

                                    continue;
                                } else {
                                    // Note this nogood is effectively removed from the watch list
                                    // of the the current predicate, since we
                                    // are only incrementing the current index, and not copying
                                    // anything to the end_index.
                                    current_index += 1;
                                    continue;
                                }
                            }

                            // Keep the current watch for this predicate.
                            self.watch_lists[update_info.0].hole[end_index] =
                                self.watch_lists[update_info.0].hole[current_index];
                            end_index += 1;
                            current_index += 1;

                            // At this point, nonwatched predicates and nogood[1] are falsified.
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy {
                                code: nogood_id.id as u64,
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
                            let nogood = &mut self.nogoods[nogood_id].predicates;

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
                            pumpkin_assert_advanced!(nogood
                                .iter()
                                .skip(1)
                                .all(|p| context.is_predicate_satisfied(*p)));

                            // There are two scenarios:
                            // nogood[0] is unassigned -> propagate the predicate to false
                            // nogood[0] is assigned true -> conflict.
                            let reason = Reason::DynamicLazy {
                                code: nogood_id.id as u64,
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

        if context.assignments.get_decision_level() == 0 {
            self.clean_up_learned_nogoods_if_needed(context);
        }
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

    /// Returns the slice representing a conjunction of predicates that explain the propagation
    /// encoded by the code, which was given to the solver by the propagator at the time of
    /// propagation.
    /// In case of the noogood propagator, lazy explanations internally also update information
    /// about the LBD and activity of the nogood, which is used when cleaning up nogoods.
    fn lazy_explanation(&mut self, code: u64, assignments: &Assignments) -> &[Predicate] {
        let id = NogoodId { id: code as u32 };
        // Update the LBD and activity of the nogood, if appropriate.
        // Note that low lbd nogoods are kept permanently, so these are not updated.
        if self.nogoods[id].is_learned && self.nogoods[id].lbd > self.parameters.lbd_threshold {
            // LBD update.
            // Note that we do not need to take into account the propagated predicate (in position
            // zero), since it will share a decision level with one of the other predicates.
            let current_lbd = self
                .lbd_helper
                .compute_lbd(&self.nogoods[id].predicates.as_slice()[1..], assignments);

            // The nogood keeps track of the best lbd encountered.
            if current_lbd < self.nogoods[id].lbd {
                self.nogoods[id].lbd = current_lbd;
                if current_lbd <= 30 {
                    self.nogoods[id].is_protected = true;
                }
            }

            // Nogood activity update.
            // Rescale the nogood activities,
            // in case bumping the activity now would lead to a large activity value.
            if self.nogoods[id].activity + self.activity_bump_increment
                > self.parameters.max_activity
            {
                self.learned_nogood_ids.high_lbd.iter().for_each(|i| {
                    self.nogoods[*i].activity /= self.parameters.max_activity;
                });
                self.activity_bump_increment /= self.parameters.max_activity;
            }

            // At this point, it is safe to increase the activity value
            self.nogoods[id].activity += self.activity_bump_increment;
        }
        // update LBD, so we need code plus assignments as input.
        &self.nogoods[id].predicates.as_slice()[1..]
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

#[derive(Default, Clone, Copy, Debug, PartialEq)]
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

    use super::NogoodPropagator;
    use crate::conjunction;
    use crate::engine::propagation::Propagator;
    use crate::engine::propagation::PropagatorId;
    use crate::engine::test_solver::TestSolver;
    use crate::predicate;
    use crate::predicates::PropositionalConjunction;
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

        let mut propagators = vec![propagator];
        let reason_lb = solver.get_reason_int_new(predicate!(b <= 0), &mut propagators);
        let reason_lb = PropositionalConjunction::from(reason_lb.to_vec());
        assert_eq!(conjunction!([a >= 2] & [c >= 10]), reason_lb);
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
