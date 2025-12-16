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
    ) -> Self {
        Self {
            propagation_method,
            propagator_options: CumulativePropagatorOptions {
                allow_holes_in_domain,
                explanation_type,
                generate_sequence,
                incremental_backtracking,
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
