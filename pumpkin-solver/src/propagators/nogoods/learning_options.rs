use clap::ValueEnum;

/// Options which determine how the learned clauses are handled .
///
/// These options influence when the learned clause database removed clauses.
#[derive(Debug, Copy, Clone)]
pub struct LearningOptions {
    /// Determines when to rescale the activites of the learned clauses in the database.
    pub max_activity: f32,
    /// Determines the factor by which the activities are divided when a conflict is found.
    pub activity_decay_factor: f32,
    /// The maximum number of clauses with an LBD higher than [`LearningOptions::lbd_threshold`]
    /// allowed by the learned clause database. If there are more clauses with an LBD higher than
    /// [`LearningOptions::lbd_threshold`] then removal from the database will be considered.
    pub limit_num_high_lbd_nogoods: usize,
    /// The treshold which specifies whether a learned clause database is considered to be with
    /// "High" LBD or "Low" LBD. Learned clauses with high LBD will be considered for removal.
    pub lbd_threshold: u32,
    /// Specifies how the learned clauses are sorted when considering removal.
    pub nogood_sorting_strategy: LearnedNogoodSortingStrategy,
    /// Specifies by how much the activity is increased when a nogood is bumped.
    pub activity_bump_increment: f32,
}
impl Default for LearningOptions {
    fn default() -> Self {
        Self {
            max_activity: 1e20,
            activity_decay_factor: 0.99,
            limit_num_high_lbd_nogoods: 4000,
            nogood_sorting_strategy: LearnedNogoodSortingStrategy::Lbd,
            lbd_threshold: 5,
            activity_bump_increment: 1.0,
        }
    }
}

/// The sorting strategy which is used when considering removal from the clause database.
#[derive(ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum LearnedNogoodSortingStrategy {
    /// Sorts based on the activity, the activity is bumped when a literal is encountered during
    /// conflict analysis.
    Activity,
    /// Sorts based on the literal block distance (LBD) which is an indication of how "good" a
    /// learned clause is.
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
