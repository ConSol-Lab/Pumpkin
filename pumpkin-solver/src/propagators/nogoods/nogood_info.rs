/// A struct which represents a nogood (i.e. a list of [`Predicate`]s which cannot all be true at
/// the same time).
///
/// It additionally contains certain fields related to how the clause was created/activity.
#[derive(Clone, Debug)]
pub(crate) struct NogoodInfo {
    /// Indicates whether the nogood is a learned nogood or not.
    pub(crate) is_learned: bool,
    /// The LBD score of the nogood; this is an indication of how "good" the nogood is.
    pub(crate) lbd: u32,
    /// Whether the nogood has been marked as deleted; this means that it can be replaced by
    /// another nogood in the future.
    pub(crate) is_deleted: bool,
    /// Whether to not allow the nogood to have their activity bumped.
    /// The idea was to prevent a single nogood from being bumped multiple times.
    /// Note that in conflict analysis, a nogood can only appear once in conflict analysis,
    /// and when using recursive minimisation, it may appear one more time, a total of two times.
    /// TODO: not clear whether this is a problem or whether it makes sense.
    pub(crate) block_bumps: bool,
    /// The activity score of the nogood.
    pub(crate) activity: f32,
}

impl NogoodInfo {
    pub(crate) fn new_learned_nogood_info(lbd: u32) -> Self {
        NogoodInfo {
            is_learned: true,
            lbd,
            is_deleted: false,
            block_bumps: false,
            activity: 0.0,
        }
    }

    pub(crate) fn new_permanent_nogood_info() -> Self {
        NogoodInfo {
            is_learned: false,
            lbd: 0,
            is_deleted: false,
            block_bumps: false,
            activity: 0.0,
        }
    }
}
