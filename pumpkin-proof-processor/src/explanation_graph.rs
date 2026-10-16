//! The predicate-level form of the inferences of one proof stage, used to eliminate auxiliary
//! variables from the inferences.

use drcp_format::ConstraintId;
use pumpkin_core::containers::HashMap;
use pumpkin_core::containers::HashSet;
use pumpkin_core::predicates::Predicate;

#[derive(Debug, Default)]
pub(crate) struct ExplanationGraph {
    /// The premises and consequent of every inference, by its ID.
    inferences: HashMap<ConstraintId, (Vec<Predicate>, Option<Predicate>)>,
    /// For every explained predicate, the inference that derived it (if any) and its reason.
    ///
    /// Implied predicates and assumptions are derived by no inference.
    derivations: HashMap<Predicate, (Option<ConstraintId>, Vec<Predicate>)>,
    /// If the conflict is an empty domain: the predicate whose propagation emptied the domain,
    /// the inference that propagated it (if any), and the reason of that propagation.
    empty_domain_conflict: Option<EmptyDomainConflict>,
}

#[derive(Clone, Debug)]
pub(crate) struct EmptyDomainConflict {
    pub(crate) trigger_predicate: Predicate,
    pub(crate) trigger_inference: Option<ConstraintId>,
    pub(crate) reason: Vec<Predicate>,
}

impl ExplanationGraph {
    pub(crate) fn clear(&mut self) {
        self.inferences.clear();
        self.derivations.clear();
        self.empty_domain_conflict = None;
    }

    pub(crate) fn add_inference(
        &mut self,
        constraint_id: ConstraintId,
        premises: &[Predicate],
        consequent: Option<Predicate>,
    ) {
        let _ = self
            .inferences
            .insert(constraint_id, (premises.to_vec(), consequent));
    }

    pub(crate) fn add_derivation(
        &mut self,
        predicate: Predicate,
        inference: Option<ConstraintId>,
        reason: &[Predicate],
    ) {
        let _ = self
            .derivations
            .insert(predicate, (inference, reason.to_vec()));
    }

    pub(crate) fn set_empty_domain_conflict(&mut self, empty_domain_conflict: EmptyDomainConflict) {
        self.empty_domain_conflict = Some(empty_domain_conflict);
    }

    pub(crate) fn get_inference(
        &self,
        constraint_id: ConstraintId,
    ) -> Option<&(Vec<Predicate>, Option<Predicate>)> {
        self.inferences.get(&constraint_id)
    }

    pub(crate) fn empty_domain_conflict(&self) -> Option<&EmptyDomainConflict> {
        self.empty_domain_conflict.as_ref()
    }

    /// Replaces every premise over an auxiliary variable by the reason it was derived from, until
    /// no premise is over an auxiliary variable.
    ///
    /// Returns the resulting premises and the IDs of the inferences that derived the replaced
    /// premises.
    pub(crate) fn resolve_auxiliary_premises(
        &self,
        premises: &[Predicate],
        is_auxiliary: impl Fn(Predicate) -> bool,
    ) -> (Vec<Predicate>, HashSet<ConstraintId>) {
        let mut resolved_premises = vec![];
        let mut used_inferences = HashSet::default();

        let mut visited: HashSet<Predicate> = HashSet::default();
        let mut to_resolve = premises.to_vec();

        while let Some(premise) = to_resolve.pop() {
            if !visited.insert(premise) {
                continue;
            }

            if !is_auxiliary(premise) {
                resolved_premises.push(premise);
                continue;
            }

            let (inference, reason) = self
                .derivations
                .get(&premise)
                .expect("every predicate used in an inference is explained");

            if let Some(constraint_id) = inference {
                let _ = used_inferences.insert(*constraint_id);
            }
            to_resolve.extend(reason.iter().copied());
        }

        resolved_premises.sort();
        (resolved_premises, used_inferences)
    }
}
