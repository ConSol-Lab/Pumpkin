use super::ConflictAnalysisNogoodContext;
use super::LearnedNogood;
use crate::engine::predicates::integer_predicate::IntegerPredicate;
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
    // current_decision_level: usize,
    // predicates: Vec<IntegerPredicate>,
}

impl AdvancedNogood {
    /// Initialises the inernal data structures for a new nogood. The information about the current
    /// decision level is used by other parts.
    pub(crate) fn new(_current_decision_level: usize) -> AdvancedNogood {
        todo!();
    }
    /// Adds the predicate to the nogood, and attaches the extra information (decision level and
    /// trail position) to the predicate internally. The extra information is used by other
    /// functions. Note that semantic redundancies is automatically applied.
    pub(crate) fn add_predicates(
        &mut self,
        _predicates: Vec<IntegerPredicate>,
        _context: &mut ConflictAnalysisNogoodContext,
    ) {
        // for predicate in predicates {
        // let decision_level = context
        // .assignments_integer
        // .get_decision_level_for_predicate(&predicate)
        // .expect("Predicates expected to be true during conflict analysis.");
        // let trail_position = context
        // .assignments_integer
        // .get_trail_position(&predicate)
        // .expect("Predicates expected to be true during conflict analysis.");
        // nogood.push(predicate, decision_level, trail_position);
        // }
        todo!();
        // println!("\t resulting nogood: {:?}", learned_nogood);

        // println!("\t after min: {:?}", learned_nogood);
    }
    /// Removes the predicate within the nogood that has the highest trail position.
    /// Returns None if the nogood is empty.
    pub(crate) fn pop_highest_trail_predicate(&mut self) -> Option<IntegerPredicate> {
        todo!();
        // let next_predicate = *learned_nogood
        // .iter()
        // .max_by_key(|predicate| {
        // context
        // .assignments_integer
        // .get_trail_position(predicate)
        // .unwrap()
        // })
        // .expect("Cannot have an empty nogood during analysis.");

        // Replace the next_predicate with its reason. This is done in two steps:
        // 1) Remove the predicate from the nogood.
        // let next_predicate_index = learned_nogood
        // .iter()
        // .position(|predicate| *predicate == next_predicate)
        // .expect("Must be able to find the predicate again.");
        // let _ = learned_nogood.swap_remove(next_predicate_index);
    }
    /// The nogood is considered propagating if it contains only one predicate from the current
    /// decision level.
    pub(crate) fn is_nogood_propagating(&self) -> bool {
        todo!();
        // todo: could be done more efficiently with additional data structures.
        // let is_nogood_propagating = |nogood: &Vec<IntegerPredicate>| {
        // let num_predicates_at_current_decision_level = num_pred_cur_dl(nogood);
        // assert!(num_predicates_at_current_decision_level > 0);
        // num_predicates_at_current_decision_level == 1
        // };

        // let num_pred_cur_dl = |nogood: &Vec<IntegerPredicate>| {
        // nogood
        // .iter()
        // .filter(|predicate| {
        // context
        // .assignments_integer
        // .get_decision_level_for_predicate(predicate)
        // .expect("Nogood predicate must have a decision level.")
        // == context.assignments_integer.get_decision_level()
        // })
        // .count()
        // };
    }
    /// Extract the internal nogood and returns it as a learned nogood.
    /// Note that this consumes the nogood.
    pub(crate) fn extract_final_learned_nogood(self) -> LearnedNogood {
        todo!();
        // todo: can be done more efficiently
        // learned_nogood
        // .sort_by_key(|predicate| context.assignments_integer.get_trail_position(predicate));
        // learned_nogood.reverse();
        //
        // sanity check
        // pumpkin_assert_moderate!(
        // context.assignments_integer.get_decision_level()
        // == context
        // .assignments_integer
        // .get_decision_level_for_predicate(&learned_nogood[0])
        // .unwrap()
        // );
        //
        // let backjump_level = if learned_nogood.len() > 1 {
        // context
        // .assignments_integer
        // .get_decision_level_for_predicate(&learned_nogood[1])
        // .unwrap()
        // } else {
        // 0
        // };
    }
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
// .assignments_integer
// .get_decision_level_for_predicate(pred)
// .unwrap(),
// context
// .assignments_integer
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
// }
// }
// }
// }
//
// remove duplicates...a bit ugly todo
// let mut new_nogood2: Vec<IntegerPredicate> = vec![];
// for predicate in &new_nogood {
// if !new_nogood2.contains(predicate) {
// new_nogood2.push(*predicate);
// }
// }
// new_nogood2
// };
