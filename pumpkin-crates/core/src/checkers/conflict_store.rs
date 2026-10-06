use pumpkin_checking::BoxedConflictChecker;
#[cfg(doc)]
use pumpkin_checking::ConflictChecker;

use crate::containers::HashMap;
use crate::predicates::Predicate;
use crate::proof::InferenceCode;

/// Owns the conflict checkers, which verify that propagations are sound.
///
/// The checkers are looked up by the inference code of a propagation.
#[derive(Clone, Debug, Default)]
pub struct ConflictCheckerStore {
    /// The conflict checkers of each inference code, with their identifiers.
    conflict_checkers:
        HashMap<InferenceCode, Vec<(ConflictCheckerId, BoxedConflictChecker<Predicate>)>>,
    /// The identifier given to the next checker that is added.
    next_id: ConflictCheckerId,
}

impl ConflictCheckerStore {
    pub fn for_inference_code(
        &self,
        inference_code: &InferenceCode,
    ) -> impl ExactSizeIterator<Item = &BoxedConflictChecker<Predicate>> {
        self.conflict_checkers
            .get(inference_code)
            .map(|checkers| itertools::Either::Left(checkers.iter().map(|(_, checker)| checker)))
            .unwrap_or(itertools::Either::Right(std::iter::empty()))
    }

    /// Add a new conflict checker for the inference code, for a constraint that is never removed.
    ///
    /// An inference code can have multiple checkers, for instance when several constraints share a
    /// constraint tag, so if a [`ConflictChecker`] was already registered for the given code,
    /// the checker is added next to the existing ones.
    pub fn add_conflict_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: BoxedConflictChecker<Predicate>,
    ) {
        let _ = self.add_removable_conflict_checker(inference_code, checker);
    }

    /// Add a new conflict checker as with [`ConflictCheckerStore::add_conflict_checker`], for a
    /// constraint that can be removed later.
    ///
    /// Returns the identifier through which [`ConflictCheckerStore::remove`] removes the checker.
    pub fn add_removable_conflict_checker(
        &mut self,
        inference_code: InferenceCode,
        checker: BoxedConflictChecker<Predicate>,
    ) -> ConflictCheckerId {
        let id = self.next_id;
        self.next_id = ConflictCheckerId(id.0 + 1);

        self.conflict_checkers
            .entry(inference_code)
            .or_default()
            .push((id, checker));

        id
    }

    /// Remove the checker with the given identifier, added for `inference_code`, for a constraint
    /// that no longer exists.
    ///
    /// The other checkers of `inference_code` are kept.
    pub fn remove(&mut self, inference_code: InferenceCode, checker: ConflictCheckerId) {
        let Some(checkers) = self.conflict_checkers.get_mut(&inference_code) else {
            return;
        };

        checkers.retain(|&(id, _)| id != checker);

        if checkers.is_empty() {
            let _ = self.conflict_checkers.remove(&inference_code);
        }
    }
}

/// Identifies a checker in the [`ConflictCheckerStore`], so that it can be removed.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ConflictCheckerId(u32);

#[cfg(test)]
mod tests {
    use pumpkin_checking::ConflictCheck;
    use pumpkin_checking::ConflictChecker;
    use pumpkin_checking::VariableState;

    use super::*;
    use crate::containers::StorageKey;
    use crate::proof::ConstraintTag;

    #[derive(Clone, Copy, Debug)]
    struct AcceptEverything;

    impl ConflictChecker<Predicate> for AcceptEverything {
        fn check(
            &self,
            _: &VariableState<Predicate>,
            _: &[Predicate],
            _: Option<&Predicate>,
        ) -> ConflictCheck {
            ConflictCheck::ConflictDetected
        }
    }

    fn accept_everything() -> BoxedConflictChecker<Predicate> {
        BoxedConflictChecker::new(Box::new(AcceptEverything))
    }

    #[test]
    fn removing_a_checker_keeps_the_others_of_its_inference_code() {
        let mut store = ConflictCheckerStore::default();
        let inference_code = InferenceCode::unknown_rule(ConstraintTag::create_from_index(0));

        let removed = store.add_removable_conflict_checker(inference_code, accept_everything());
        store.add_conflict_checker(inference_code, accept_everything());

        store.remove(inference_code, removed);

        assert_eq!(store.for_inference_code(&inference_code).len(), 1);
    }

    #[test]
    fn removing_the_last_checker_of_an_inference_code_leaves_none() {
        let mut store = ConflictCheckerStore::default();
        let inference_code = InferenceCode::unknown_rule(ConstraintTag::create_from_index(0));

        let removed = store.add_removable_conflict_checker(inference_code, accept_everything());

        store.remove(inference_code, removed);

        assert_eq!(store.for_inference_code(&inference_code).len(), 0);
    }
}
