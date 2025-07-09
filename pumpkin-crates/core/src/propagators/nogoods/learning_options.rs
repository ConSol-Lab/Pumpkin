/// Options related to nogood management, i.e., how and when to remove learned nogoods from the
/// database.
#[derive(Debug, Copy, Clone)]
pub struct LearningOptions {
    /// Determines when to rescale the activites of the learned nogoods in the database.
    pub max_activity: f32,
    /// Determines the factor by which the activities are divided when a conflict is found.
    pub activity_decay_factor: f32,
    /// The solver partitions the nogoods into three tiers. Each tier can store a predefined
    /// number of nogoods. The limits below determine those limits.
    /// If there are more nogoods than allowed, nogood clean up is initiated.
    pub limit_high_lbd_nogoods: usize,
    pub limit_mid_lbd_nogoods: usize,
    pub limit_low_lbd_nogoods: usize,
    /// Thresholds used to determine the tier of a nogood during learned nogood clean up:
    /// 'low-lbd-tier' ("core tier") when 'lbd <= lbd_low',
    /// 'mid-lbd-tier' when 'lbd_low_threshold < lbd < lbd_high, and
    /// 'high-lbd-tier' ("local tier") when 'lbd >= lbd_high'
    pub lbd_high: u32,
    pub lbd_low: u32,
    /// Specifies by how much the activity is increased when a nogood is bumped.
    pub activity_bump_increment: f32,
}
impl Default for LearningOptions {
    fn default() -> Self {
        Self {
            max_activity: 1e20,
            activity_decay_factor: 0.99,
            limit_high_lbd_nogoods: 20000,
            limit_mid_lbd_nogoods: 7000,
            limit_low_lbd_nogoods: 100000,
            lbd_high: 7,
            lbd_low: 3,
            activity_bump_increment: 1.0,
        }
    }
}

/// The sorting strategy which is used when considering removal from the clause database.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub(crate) enum LearnedNogoodSortingStrategy {
    /// Sorts based on the activity, the activity is bumped when a literal is encountered during
    /// conflict analysis.
    #[default]
    Activity,
    /// Sorts based on the literal block distance (LBD) which is an indication of how "good" a
    /// learned clause is.
    Lbd,
}
