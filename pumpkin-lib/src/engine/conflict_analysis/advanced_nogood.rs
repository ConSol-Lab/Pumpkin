use super::ConflictAnalysisNogoodContext;
use super::LearnedNogood;
use crate::basic_types::HashMap;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
use crate::engine::variables::DomainId;
use crate::engine::variables::IntegerVariable;
use crate::engine::Assignments;
use crate::predicate;
use crate::pumpkin_assert_moderate;
/// The struct represents a nogood that does additional bookkeeping. The nogood is used during
/// conflict analysis as the learned nogood. The core functionality is as follows:
/// * Stores predicates, as expected from a nogood.
/// * Automatically minimises the nogood by removing semantic redundancies. For example, if the
///   nogood is '[x >= k]', then adding '[x != k - 2]' will have no effect, since this the added
///   predicate is redundant. During conflict analysis we expect that we may encounter many
///   redundancies, so this helps derive stronger nogoods.
/// * Allows popping the predicate that has the highest position on the trail. This is used as the
///   next candidate in conflict analysis.
/// * Reports whether the nogood is propagating, i.e., has only one predicate from the current
///   decision level.
pub(crate) struct AdvancedNogood {
    current_decision_level: usize,
    pub(crate) predicates: Vec<PredicateWithInfo>,
    /// Used to do semantic minimisation.
    internal_assignments: Assignments,
    /// Incoming predicate domain ids are mapped to an internal representation, which is used in
    /// the internal assignments data structure.
    internal_ids: HashMap<DomainId, DomainId>,
    // note to self: could optimise by separately tracking predicates from the current decision
    // level and those at lower levels. This is because we only need consider the ones at the
    // current decision level when replacing predicates with their reason.
}

impl AdvancedNogood {
    /// Initialises the internal data structures for a new nogood.
    /// The information about the current decision level is used by other parts.
    pub(crate) fn new(current_decision_level: usize) -> AdvancedNogood {
        AdvancedNogood {
            current_decision_level,
            predicates: vec![],
            internal_assignments: Assignments::default(),
            internal_ids: HashMap::default(),
        }
    }

    fn register_id_internally(
        &mut self,
        domain_id: DomainId,
        context: &ConflictAnalysisNogoodContext,
    ) {
        // Only register if the domain is not yet registered.
        // I am disabling the clippy warning temporarily.
        #[allow(clippy::map_entry)]
        if !self.internal_ids.contains_key(&domain_id) {
            // Internally the domain_id is replicated but with an internal id.

            // Collect root level state of the domain_id
            let lower_bound = context.assignments.get_initial_lower_bound(domain_id);
            let upper_bound = context.assignments.get_initial_upper_bound(domain_id);
            let holes = context.assignments.get_initial_holes(domain_id);

            // Replicate the domain with the new internal id
            let internal_domain_id = self.internal_assignments.grow(lower_bound, upper_bound);
            for hole in holes {
                internal_domain_id
                    .remove(&mut self.internal_assignments, hole, None)
                    .expect("Cannot fail with root removals.");
            }

            let _ = self.internal_ids.insert(domain_id, internal_domain_id);
        }
    }

    fn get_internal_id(&self, domain_id: DomainId) -> DomainId {
        *self.internal_ids.get(&domain_id).unwrap()
    }

    fn convert_into_internal_predicate(
        &mut self,
        predicate: IntegerPredicate,
        context: &ConflictAnalysisNogoodContext,
    ) -> IntegerPredicate {
        self.register_id_internally(predicate.get_domain(), context);
        let internal_id = self.get_internal_id(predicate.get_domain());
        // The code below is cumbersome, but this is the idea:
        // The internal predicate is the same as the input predicate,
        // but swapping out the domain id for the internal domain id.
        match predicate {
            IntegerPredicate::LowerBound {
                domain_id: _,
                lower_bound,
            } => predicate![internal_id >= lower_bound],
            IntegerPredicate::UpperBound {
                domain_id: _,
                upper_bound,
            } => predicate![internal_id <= upper_bound],
            IntegerPredicate::NotEqual {
                domain_id: _,
                not_equal_constant,
            } => predicate![internal_id != not_equal_constant],
            IntegerPredicate::Equal {
                domain_id: _,
                equality_constant,
            } => predicate![internal_id == equality_constant],
        }
    }

    fn debug_check_duplicates(&self) -> bool {
        for entry in &self.predicates {
            assert!(
                self.predicates
                    .iter()
                    .filter(|p| p.predicate == entry.predicate)
                    .count()
                    == 1,
                "There cannot be duplicate predicates."
            );
        }
        true
    }

    fn add_predicate(
        &mut self,
        predicate: IntegerPredicate,
        context: &mut ConflictAnalysisNogoodContext,
    ) {
        let internal_predicate = self.convert_into_internal_predicate(predicate, context);

        match self
            .internal_assignments
            .evaluate_predicate(internal_predicate)
        {
            Some(truth_value) => match truth_value {
                true => {
                    // println!("true");
                    // The predicate already satisfied, so it can be ignored.
                }
                false => {
                    // The predicate is falsified, which cannot happen during conflict analysis.
                    unreachable!();
                }
            },
            None => {
                // The predicate is undecided, so we can post it.
                self.internal_assignments
                    .post_integer_predicate(internal_predicate, None)
                    .expect("Previous code asserted this will be a success.");

                // Add the predicate to the list of predicates in the nogood.
                let decision_level = context
                    .assignments
                    .get_decision_level_for_predicate(&predicate)
                    .unwrap();

                let trail_position = context.assignments.get_trail_position(&predicate).unwrap();

                self.predicates.push(PredicateWithInfo {
                    predicate,
                    decision_level,
                    trail_position,
                });
                pumpkin_assert_moderate!(self.debug_check_duplicates());
                // Note that the added predicate may make some of the already present predicates
                // redundant. We do not remove redundant predicates now, but leave this for the end.
            }
        }
    }

    /// Adds the predicate to the nogood, and attaches the extra information (decision level and
    /// trail position) to the predicate internally. The extra information is used by other
    /// functions. Note that semantic redundancies is automatically applied.
    pub(crate) fn add_predicates(
        &mut self,
        predicates: Vec<IntegerPredicate>,
        context: &mut ConflictAnalysisNogoodContext,
    ) {
        for predicate in predicates {
            // println!("\tp: {}", predicate);
            // if !context
            // .assignments
            // .evaluate_predicate(predicate)
            // .is_some_and(|x| x)
            // {
            // println!("\tNot ok: {:?}", predicate);
            // }

            assert!(
                context
                    .assignments
                    .evaluate_predicate(predicate)
                    .is_some_and(|x| x),
                "Predicates must be true during conflict analysis."
            );
            self.add_predicate(predicate, context);

            // Currently we notify of every predicate. It may be better to only do so if the
            // predicate is not subsumed.
            context
                .brancher
                .on_appearance_in_conflict_predicate(predicate);

            // println!("\tafter add: {:?}", self.predicates);
        }
        self.simplify_predicates(context);
        // println!("simply {:?}", self.predicates);
        // println!("\t resulting nogood: {:?}", learned_nogood);
        // println!("\t after min: {:?}", learned_nogood);
    }
    /// Removes the predicate that has the highest trail position.
    /// Returns None if the nogood is empty.
    pub(crate) fn pop_highest_trail_predicate(&mut self) -> Option<IntegerPredicate> {
        assert!(
            !self.predicates.is_empty(),
            "Do not expect to see an empty nogood during conflict analysis!"
        );
        // For now we do this using several linear passes.
        // In the future could consider doing this more efficiently,
        // a single linear scan or using a heap-like structure.

        // If there is at least one element left.
        if let Some(next_predicate) = self.predicates.iter().max_by_key(|p| p.trail_position) {
            // Find the position of the next predicate.
            // This is a bit non-elegant but okay for now.
            let position = self
                .predicates
                .iter()
                .position(|p| p.trail_position == next_predicate.trail_position)
                .unwrap();

            // I think this is not true: it could be that two predicates have the same trail
            // position because they have been implicit set to true due to the posted predicate.
            assert!(
                self.predicates[position].predicate == next_predicate.predicate
                    && self
                        .predicates
                        .iter()
                        .filter(|p| p.trail_position == next_predicate.trail_position)
                        .count()
                        == 1,
                "Sanity check that the trail positions are unique."
            );
            let removed_predicate = self.predicates.swap_remove(position);
            Some(removed_predicate.predicate)
        } else {
            None
        }
    }
    /// The nogood is considered propagating if it contains only one predicate from the current
    /// decision level.
    pub(crate) fn is_nogood_propagating(&self) -> bool {
        // Could be done more efficiently with additional data structures.
        let num_predicates_at_current_decision_level = self
            .predicates
            .iter()
            .filter(|p| p.decision_level == self.current_decision_level)
            .count();

        assert!(
            num_predicates_at_current_decision_level > 0,
            "Sanity check."
        );
        // The nogood is propagating if it has exactly one predicate from the current decision
        // level.
        num_predicates_at_current_decision_level == 1
    }

    /// Extract the internal nogood and returns it as a learned nogood.
    /// It is guaranteed that the predicates in position zero is the propagating predicate,
    /// and the position one contains the predicate with the second highest decision level.
    /// This is needed when adding the learned nogood.
    pub(crate) fn extract_final_learned_nogood(&self) -> LearnedNogood {
        // The predicates in the nogood are sorted according to trail position.
        // This is too strong and could be relaxed later.

        // Ideally we would use the retain function of the vector, however we cannot due to borrow
        // checker rules. So we do a manual non-inplace version.
        Self::prepare_learned_nogood(self.predicates.clone(), self.current_decision_level)
    }

    fn simplify_predicates(&mut self, context: &ConflictAnalysisNogoodContext) {
        // I think this is correct, todo double check.

        // The predicates are determined based on the assignments.
        // There may be more efficient ways to implement this.
        // Linearly go through the predicates, and check whether the predicate should be kept.
        // Determining if it should be kept is done as follows.
        // 1) [x == k] and not already kept: keep.
        // 2) [x != k] and within bounds of assignment: keep.
        // 3) [x >= k] _and_ not root predicate _and_ variable x not assigned: keep.
        // 4) [x <= k] _and_ not root predicate _and_ variable x not assigned: keep.
        // 5) [x <= k] _and_ not root predicate _and_ variable x _is_ assigned: add [x == k] if it
        //    is not already present. Do not add the bound predicate.
        // 6) [x >= k] _and not root predicate _and_ variable x _is_ assigned: add [x == k] if it is
        //    not already present. Do not add the bound predicate.

        // Only consider the ids that appear in the predicates.
        // The reason for not consider all internal ids is that the conflict
        // analysis may have determined that some of the predicates are redundant, so we only look
        // at predicates that remain in the list.

        // Note that the self.predicates and self.assignments have different ids, that need to be
        // converted via self.internal_ids.

        let mut simplified_predicates: Vec<PredicateWithInfo> = vec![];
        for p in &self.predicates {
            let internal_id = self.get_internal_id(p.predicate.get_domain());
            match p.predicate {
                IntegerPredicate::LowerBound {
                    domain_id,
                    lower_bound,
                } => {
                    // 3) [x >= k] _and_ not root predicate _and_ variable x not assigned: keep.
                    // 6) [x >= k] _and not root predicate _and_ variable x _is_ assigned: add [x ==
                    //    k] if it is not already present. Do not add the bound predicate.

                    // first check that it is not a root predicate
                    if self
                        .internal_assignments
                        .get_initial_lower_bound(internal_id)
                        < lower_bound
                    {
                        match self.internal_assignments.is_domain_assigned(internal_id) {
                            true => {
                                // Important to note that the assigned value may be different than
                                // the one given by the lower bound (due to holes).
                                let assigned_value = self
                                    .internal_assignments
                                    .get_assigned_value(internal_id)
                                    .unwrap();
                                if simplified_predicates.iter().all(|simplified_entry| {
                                    simplified_entry.predicate
                                        != predicate![domain_id == assigned_value]
                                }) {
                                    let original_id = p.predicate.get_domain();
                                    // Be careful not to mixed up internal_domain_id and domain_id.
                                    let decision_level = context
                                        .assignments
                                        .get_decision_level_for_predicate(&predicate![
                                            original_id == assigned_value
                                        ])
                                        .unwrap();
                                    let trail_position = context
                                        .assignments
                                        .get_trail_position(&predicate![
                                            original_id == assigned_value
                                        ])
                                        .unwrap();
                                    // Add the predicate to the simplified list.
                                    simplified_predicates.push(PredicateWithInfo {
                                        predicate: predicate![domain_id == assigned_value],
                                        decision_level,
                                        trail_position,
                                    });
                                }
                            }
                            false => {
                                // Here it is important to note that even though this predicate is a
                                // lower bound, due to holes, the
                                // lower bound value given by the predicate may not be the strongest
                                // lower bound value, so we need to query it from the assignments.
                                // Add the predicate to the simplified list.
                                let original_id = p.predicate.get_domain();
                                let strongest_lower_bound =
                                    self.internal_assignments.get_lower_bound(internal_id);
                                let decision_level = context
                                    .assignments
                                    .get_decision_level_for_predicate(&predicate![
                                        original_id >= strongest_lower_bound
                                    ])
                                    .unwrap();
                                let trail_position = context
                                    .assignments
                                    .get_trail_position(&predicate![
                                        original_id >= strongest_lower_bound
                                    ])
                                    .unwrap();
                                simplified_predicates.push(PredicateWithInfo {
                                    predicate: predicate![domain_id >= strongest_lower_bound],
                                    decision_level,
                                    trail_position,
                                });
                            }
                        }
                    }
                }
                IntegerPredicate::UpperBound {
                    domain_id,
                    upper_bound,
                } => {
                    // 4) [x <= k] _and_ not root predicate _and_ variable x not assigned: keep.
                    // 5) [x <= k] _and_ not root predicate _and_ variable x _is_ assigned:
                    // add [x == k] if it is not already present. Do not add the bound predicate.

                    // first check that it is not a root predicate
                    if self
                        .internal_assignments
                        .get_initial_upper_bound(internal_id)
                        > upper_bound
                    {
                        match self.internal_assignments.is_domain_assigned(internal_id) {
                            true => {
                                // Important to note that the assigned value may be different than
                                // the one given by the lower bound (due to holes).
                                let assigned_value = self
                                    .internal_assignments
                                    .get_assigned_value(internal_id)
                                    .unwrap();
                                if simplified_predicates.iter().all(|simplified_entry| {
                                    simplified_entry.predicate
                                        != predicate![domain_id == assigned_value]
                                }) {
                                    // Be careful not to mixed up internal_domain_id and domain_id.
                                    let original_id = p.predicate.get_domain();
                                    let decision_level = context
                                        .assignments
                                        .get_decision_level_for_predicate(&predicate![
                                            original_id == assigned_value
                                        ])
                                        .unwrap();
                                    let trail_position = context
                                        .assignments
                                        .get_trail_position(&predicate![
                                            original_id == assigned_value
                                        ])
                                        .unwrap();
                                    // Add the predicate to the simplified list.
                                    simplified_predicates.push(PredicateWithInfo {
                                        predicate: predicate![domain_id == assigned_value],
                                        decision_level,
                                        trail_position,
                                    });
                                }
                            }
                            false => {
                                // Here it is important to note that even though this predicate is a
                                // upper bound, due to holes, the
                                // upper bound value given by the predicate may not be the strongest
                                // upper bound value, so we need to query it from the assignments.
                                // Add the predicate to the simplified list.
                                let original_id = p.predicate.get_domain();
                                let strongest_upper_bound =
                                    self.internal_assignments.get_upper_bound(internal_id);
                                let decision_level = context
                                    .assignments
                                    .get_decision_level_for_predicate(&predicate![
                                        original_id <= strongest_upper_bound
                                    ])
                                    .unwrap();
                                let trail_position = context
                                    .assignments
                                    .get_trail_position(&predicate![
                                        original_id <= strongest_upper_bound
                                    ])
                                    .unwrap();
                                simplified_predicates.push(PredicateWithInfo {
                                    predicate: predicate![domain_id <= strongest_upper_bound],
                                    decision_level,
                                    trail_position,
                                });
                            }
                        }
                    }
                }
                IntegerPredicate::NotEqual {
                    domain_id: _,
                    not_equal_constant,
                } => {
                    // 2) [x != k] and within bounds of assignment: keep.
                    if self.internal_assignments.get_lower_bound(internal_id) < not_equal_constant
                        && not_equal_constant
                            < self.internal_assignments.get_upper_bound(internal_id)
                    {
                        simplified_predicates.push(*p);
                    }
                }
                IntegerPredicate::Equal {
                    domain_id: _,
                    equality_constant: _,
                } => {
                    // 1) [x == k] and not already kept: keep.
                    // Note that in this case, the equality constant is the right value to search
                    // for, since contrary to the case for lower and upper bounds, holes cannot
                    // influence this value.
                    if simplified_predicates
                        .iter()
                        .all(|simplified_entry| *simplified_entry != *p)
                    {
                        simplified_predicates.push(*p);
                    }
                }
            }
        }
        self.predicates = simplified_predicates;
    }

    /// Orders the predicates in correct position and compute the backjump level.
    fn prepare_learned_nogood(
        mut nogood: Vec<PredicateWithInfo>,
        current_decision_level: usize,
    ) -> LearnedNogood {
        // Sorting does the trick with placing the correct predicates at the first two positions,
        // however this can be done more efficiently.
        nogood.sort_by_key(|p| p.trail_position);
        nogood.reverse();

        // sanity check
        pumpkin_assert_moderate!(
            current_decision_level == nogood[0].decision_level || nogood.len() == 1,
            "Sanity check."
        );

        // The second highest decision level predicate is at position one.
        // This is the backjump level.
        let backjump_level = if nogood.len() > 1 {
            nogood[1].decision_level
        }
        // For unit nogoods, the solver backtracks to the root level.
        else {
            0
        };

        let predicates = nogood.iter().map(|p| p.predicate).collect();

        LearnedNogood {
            predicates,
            backjump_level,
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) struct PredicateWithInfo {
    predicate: IntegerPredicate,
    decision_level: usize,
    trail_position: usize,
}

// println!("Conflict nogood: {:?}", learned_nogood);
// pumpkin_assert_moderate!(!is_nogood_propagating(&learned_nogood)); -> not true

// println!("Conflict nogood minimised: {:?}", learned_nogood);
//
// println!("num curr: {}", num_pred_cur_dl(&learned_nogood));
//
// for pred in &learned_nogood {
// println!(
// "\t{} {} {}",
// pred,
// context
// .assignments
// .get_decision_level_for_predicate(pred)
// .unwrap(),
// context
// .assignments
// .get_trail_position(pred)
// .unwrap()
// );
// }

// Currently we need to use this minimiser. The problem stems from the fact that when we
// make assignments, we make them as two updates: one for the lower bound, and one
// for the upper bound. This means that we may have _two_ decision predicates at the given
// level: this is a problem for the learning stopping criteria, because it stops when only
// one predicate from the current decision level, but when we can make these
// compound decisions, then this does not work with that criteria.
// So as a work around we do this minimisation to merge two predicate literals into one, and
// only do it for the current decision level if there are two predicates left.

// let semantic_minimiser_draft = |nogood: &Vec<IntegerPredicate>| -> Vec<IntegerPredicate> {
// the idea is to collect all the bounds present,
// then replace predicates if the bound meet.
// let mut lbs: HashMap<DomainId, i32> = HashMap::default();
// let mut ubs: HashMap<DomainId, i32> = HashMap::default();
// first collect bounds
// for predicate in nogood {
// match predicate {
// IntegerPredicate::LowerBound {
// domain_id,
// lower_bound: predicate_bound,
// } => {
// if let Some(stored_bound) = lbs.get_mut(domain_id) {
// let better_bound = cmp::max(*predicate_bound, *stored_bound);
// stored_bound = better_bound;
// } else {
// let _ = lbs.insert(*domain_id, *predicate_bound);
// }
// }
// IntegerPredicate::UpperBound {
// domain_id,
// upper_bound: predicate_bound,
// } => {
// if let Some(stored_bound) = ubs.get_mut(domain_id) {
// let better_bound = cmp::min(*predicate_bound, *stored_bound);
// stored_bound = better_bound;
// } else {
// let _ = ubs.insert(*domain_id, *predicate_bound);
// }
// }
// IntegerPredicate::NotEqual {
// domain_id: _,
// not_equal_constant: _,
// } => { // do nothing
// }
// IntegerPredicate::Equal {
// domain_id,
// equality_constant,
// } => {
// if let Some(stored_bound) = lbs.get_mut(domain_id) {
// assert!(*stored_bound <= *equality_constant);
// stored_bound = *equality_constant;
// } else {
// let _ = lbs.insert(*domain_id, *equality_constant);
// }
//
// if let Some(stored_bound) = ubs.get_mut(domain_id) {
// assert!(*stored_bound >= *equality_constant);
// stored_bound = *equality_constant;
// } else {
// let _ = ubs.insert(*domain_id, *equality_constant);
// }
// }
// }
// }
//
// let is_assigned = |predicate: &IntegerPredicate| -> bool {
// let d = predicate.get_domain();
// if let Some(lb) = lbs.get(&d) {
// if let Some(ub) = ubs.get(&d) {
// if *lb == *ub {
// return true;
// }
// }
// }
// false
// };
// now rewrite the predicates
// let mut already_added_equality: HashSet<DomainId> = HashSet::default();
// let mut new_nogood: Vec<IntegerPredicate> = vec![];
// for predicate in nogood {
// match predicate {
// IntegerPredicate::LowerBound {
// domain_id,
// lower_bound: _,
// } => {
// if is_assigned(predicate) {
// do nothing
// the upper bound predicate will add the equality predicate
// }
// otherwise the predicate is not fixed, so add the better version of it
// else if lbs.contains_key(domain_id) {
// new_nogood.push(IntegerPredicate::LowerBound {
// domain_id: *domain_id,
// lower_bound: *lbs.get(domain_id).unwrap(),
// })
// } else {
// new_nogood.push(*predicate);
// }
// }
// IntegerPredicate::UpperBound {
// domain_id,
// upper_bound: _,
// } => {
// if is_assigned(predicate) {
// if !already_added_equality.contains(domain_id) {
// new_nogood.push(IntegerPredicate::Equal {
// domain_id: *domain_id,
// equality_constant: *ubs.get(domain_id).unwrap(),
// });
// let _ = already_added_equality.insert(*domain_id);
// }
// }
// otherwise the predicate is not fixed, so add the better version of it
// else if ubs.contains_key(domain_id) {
// new_nogood.push(IntegerPredicate::UpperBound {
// domain_id: *domain_id,
// upper_bound: *ubs.get(domain_id).unwrap(),
// })
// } else {
// new_nogood.push(*predicate);
// }
// }
// IntegerPredicate::NotEqual {
// domain_id,
// not_equal_constant,
// } => {
// only add the not equal if it is within the bounds
// todo: now we do not remove corner values...
// let mut should_add = true;
// if let Some(lb) = lbs.get(domain_id) {
// if lb > not_equal_constant {
// should_add = false;
// }
// }
//
// if let Some(ub) = ubs.get(domain_id) {
// if ub < not_equal_constant {
// should_add = false;
// }
// }
//
// if should_add {
// new_nogood.push(*predicate);
// }
// }
// IntegerPredicate::Equal {
// domain_id,
// equality_constant: _,
// } => {
// if !already_added_equality.contains(domain_id) {
// new_nogood.push(*predicate);
// let _ = already_added_equality.insert(*domain_id);
