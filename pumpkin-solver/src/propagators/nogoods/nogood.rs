use crate::predicates::Predicate;
use crate::predicates::PropositionalConjunction;

/// A struct which represents a nogood (i.e. a list of [`Predicate`]s which cannot all be true at
/// the same time).
///
/// It additionally contains certain fields related to how the clause was created/activity.
#[derive(Clone, Debug)]
pub(crate) struct Nogood {
    /// The predicates which are part of the nogood.
    pub(crate) predicates: PropositionalConjunction,
    /// Indicates whether the nogood is a learned nogood or not.
    pub(crate) is_learned: bool,
    /// The LBD score of the nogood; this is an indication of how "good" the nogood is.
    pub(crate) lbd: u32,
    /// If a nogood is protected then it is not considered for removal for a single iteration.
    pub(crate) is_protected: bool,
    /// Whether the nogood has been marked as deleted; this means that it can be replaced by
    /// another nogood in the future.
    pub(crate) is_deleted: bool,
    /// Whether to not allow the nogood to have their activity bumped.
    pub(crate) block_bumps: bool,
    /// The activity score of the nogood.
    pub(crate) activity: f32,
    /// The predicate which was last found to be falsified; we store this predicate to allow for
    /// simple checking of whether a nogood might be satisfied
    pub(crate) cached_predicate: Predicate,
}

impl Default for Nogood {
    fn default() -> Self {
        Self {
            predicates: Default::default(),
            is_learned: Default::default(),
            lbd: Default::default(),
            is_protected: Default::default(),
            is_deleted: Default::default(),
            block_bumps: Default::default(),
            activity: Default::default(),
            // We select a predicate which is always satified as the default
            cached_predicate: Predicate::trivially_true(),
        }
    }
}

impl Nogood {
    pub(crate) fn new_learned_nogood(predicates: PropositionalConjunction, lbd: u32) -> Self {
        Nogood {
            predicates,
            is_learned: true,
            lbd,
            ..Default::default()
        }
    }

    pub(crate) fn new_permanent_nogood(predicates: PropositionalConjunction) -> Self {
        Nogood {
            predicates,
            ..Default::default()
        }
    }
}
