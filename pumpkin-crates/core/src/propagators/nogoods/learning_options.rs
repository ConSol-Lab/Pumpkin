/// Options related to nogood management, i.e., how and when to remove learned nogoods from the
/// database.
#[derive(Debug, Copy, Clone)]
pub struct LearningOptions {
    /// Determines when to rescale the activites of the learned nogoods in the database.
    pub max_activity: f32,
    /// Determines the factor by which the activities are divided when a conflict is found.
    pub activity_decay_factor: f32,
    /// The maximum number of nogoods that the solver stores
    pub current_max_num_nogoods: usize,
    /// The factor by which the maximum number of nogoods is increased after each database
    /// reduction
    pub max_num_nogoods_increment: f32,
    /// The upper limit for the number of stored nogoods
    pub num_nogoods_upper_limit: usize,
    /// Specifies by how much the activity is increased when a nogood is bumped.
    pub activity_bump_increment: f32,
}
impl Default for LearningOptions {
    fn default() -> Self {
        Self {
            max_activity: 1e20,
            activity_decay_factor: 0.99,
            current_max_num_nogoods: 40000,
            num_nogoods_upper_limit: 1_000_000_000,
            max_num_nogoods_increment: 1.1,
            activity_bump_increment: 1.0,
        }
    }
}
