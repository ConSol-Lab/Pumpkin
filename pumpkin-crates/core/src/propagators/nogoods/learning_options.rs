/// Options related to nogood management, i.e., how and when to remove learned nogoods from the
/// database.
#[derive(Debug, Copy, Clone)]
pub struct LearningOptions {
    /// Determines when to rescale the activites of the learned nogoods in the database.
    pub max_activity: f32,
    /// Determines the factor by which the activities are divided when a conflict is found.
    pub activity_decay_factor: f32,
    /// The solver partitions the nogoods into three tiers.
    ///
    /// This limit specifies how many nogoods can be stored in the "high" LBD tier.
    pub max_num_high_lbd_nogoods: usize,
    /// The solver partitions the nogoods into three tiers.
    ///
    /// This limit specifies how many nogoods can be stored in the "mid" LBD tier.
    pub max_num_mid_lbd_nogoods: usize,
    /// The solver partitions the nogoods into three tiers.
    ///
    /// This limit specifies how many nogoods can be stored in the "low" LBD tier.
    pub max_num_low_lbd_nogoods: usize,
    /// Used to determine which tier a nogood belongs in.
    ///
    /// If the LBD of a nogood is higher than or equal to this threshold then it is considered to
    /// be a "high" LBD nogood.
    ///
    /// If the LBD of a nogood is between [`LearningOptions::lbd_threshold_high`] and
    /// [`LearningOptions::lbd_threshold_low`] then it is considered a "mid" LBD nogood.
    pub lbd_threshold_high: u32,
    /// Used to determine which tier a nogood belongs in.
    ///
    /// If the LBD of a nogood is lower than or equal to this value then it is considered to be a
    /// "low" LBD nogood.
    ///
    /// If the LBD of a nogood is between [`LearningOptions::lbd_threshold_high`] and
    /// [`LearningOptions::lbd_threshold_low`] then it is considered a "mid" LBD nogood.
    pub lbd_threshold_low: u32,
    /// Specifies by how much the activity is increased when a nogood is bumped.
    pub activity_bump_increment: f32,
}
impl Default for LearningOptions {
    fn default() -> Self {
        Self {
            max_activity: 1e20,
            activity_decay_factor: 0.99,
            max_num_high_lbd_nogoods: 20000,
            max_num_mid_lbd_nogoods: 7000,
            max_num_low_lbd_nogoods: 100000,
            lbd_threshold_high: 7,
            lbd_threshold_low: 3,
            activity_bump_increment: 1.0,
        }
    }
}
