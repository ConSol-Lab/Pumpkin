/// A struct which represents the flags of a nogood.
#[derive(Default, Clone, Debug)]
pub(crate) struct NogoodInfo {
    /// Indicates whether the nogood is a learned nogood or not.
    pub(crate) is_learned: bool,
    /// If a nogood is protected then it is not considered for removal for a single iteration.
    pub(crate) is_protected: bool,
    /// Whether the nogood has been marked as deleted; this means that it can be replaced by
    /// another nogood in the future.
    pub(crate) is_deleted: bool,
    /// Whether to not allow the nogood to have their activity bumped.
    pub(crate) block_bumps: bool,
    /// The activity score of the nogood.
    pub(crate) activity: f32,
    /// The LBD score of the nogood; this is an indication of how "good" the nogood is.
    pub(crate) lbd: u32,
}

impl NogoodInfo {
    pub(crate) fn new_learned_nogood(lbd: u32) -> Self {
        Self {
            is_learned: true,
            lbd,
            ..Default::default()
        }
    }
}
