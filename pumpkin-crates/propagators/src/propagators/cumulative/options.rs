use crate::cumulative::time_table::CumulativeExplanationType;

#[derive(Debug, Default, Clone, Copy)]
pub struct CumulativePropagatorOptions {
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub allow_holes_in_domain: bool,
    /// The type of explanation which is used by the cumulative to explain propagations and
    /// conflicts.
    pub explanation_type: CumulativeExplanationType,
    /// Determines whether a sequence of profiles is generated when explaining a propagation.
    pub generate_sequence: bool,
    /// Determines whether to incrementally backtrack or to calculate from scratch
    pub incremental_backtracking: bool,
    /// Determines when to merge when using time-table reasoning.
    pub merge_strategy: CumulativeMergeStrategy,
    /// If [`CumulativeMergeStrategy::Constant`] is used, then this parameter indicates the number
    /// of propagation calls after which a new merge will take place.
    pub merge_strategy_constant: Option<u32>,
}

/// The options provided to the Cumulative constraint.
#[derive(Debug, Copy, Clone, Default)]
pub struct CumulativeOptions {
    /// The propagation method which is used for the cumulative constraints; currently all of them
    /// are variations of time-tabling. The default is incremental time-tabling reasoning over
    /// intervals.
    pub propagation_method: CumulativePropagationMethod,
    /// The options which are passed to the propagator itself
    pub propagator_options: CumulativePropagatorOptions,
}

impl CumulativeOptions {
    pub fn new(
        allow_holes_in_domain: bool,
        explanation_type: CumulativeExplanationType,
        generate_sequence: bool,
        propagation_method: CumulativePropagationMethod,
        incremental_backtracking: bool,
        merge_strategy: CumulativeMergeStrategy,
        merge_strategy_constant: Option<u32>,
    ) -> Self {
        Self {
            propagation_method,
            propagator_options: CumulativePropagatorOptions {
                allow_holes_in_domain,
                explanation_type,
                generate_sequence,
                incremental_backtracking,
                merge_strategy,
                merge_strategy_constant,
            },
        }
    }
}

/// The approach used for propagating the Cumulative constraint.
#[derive(Debug, Default, Clone, Copy)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum CumulativePropagationMethod {
    TimeTablePerPoint,
    TimeTablePerPointIncremental,
    TimeTablePerPointIncrementalSynchronised,
    TimeTableOverInterval,
    #[default]
    TimeTableOverIntervalIncremental,
    TimeTableOverIntervalIncrementalSynchronised,
}

/// The strategy to use when merging
#[derive(Debug, Default, Clone, Copy)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum CumulativeMergeStrategy {
    Constant,
    Average,
    #[default]
    Never,
    Always,
}
