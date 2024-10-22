use std::fmt::Display;

use super::CumulativeExplanationType;

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct CumulativePropagatorOptions {
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub(crate) allow_holes_in_domain: bool,
    /// The type of explanation which is used by the cumulative to explain propagations and
    /// conflicts.
    pub(crate) explanation_type: CumulativeExplanationType,
    /// Determines whether a sequence of profiles is generated when explaining a propagation.
    pub(crate) generate_sequence: bool,
    /// Determines whether to incrementally backtrack or to calculate from scratch
    pub(crate) incremental_backtracking: bool,
}

#[derive(Debug, Copy, Clone, Default)]
pub struct CumulativeOptions {
    /// The propagation method which is used for the cumulative constraints; currently all of them
    /// are variations of time-tabling. The default is incremental time-tabling reasoning over
    /// intervals.
    pub(crate) propagation_method: CumulativePropagationMethod,
    /// The options which are passed to the propagator itself
    pub(crate) propagator_options: CumulativePropagatorOptions,
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

#[derive(Debug, Default, Clone, Copy)]
pub enum CumulativePropagationMethod {
    TimeTablePerPoint,
    TimeTablePerPointIncremental,
    TimeTablePerPointIncrementalSynchronised,
    TimeTableOverInterval,
    #[default]
    TimeTableOverIntervalIncremental,
    TimeTableOverIntervalIncrementalSynchronised,
}

impl Display for CumulativePropagationMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CumulativePropagationMethod::TimeTablePerPoint => write!(f, "time-table-per-point"),
            CumulativePropagationMethod::TimeTablePerPointIncremental => {
                write!(f, "time-table-per-point-incremental")
            }
            CumulativePropagationMethod::TimeTablePerPointIncrementalSynchronised => {
                write!(f, "time-table-per-point-incremental-synchronised")
            }
            CumulativePropagationMethod::TimeTableOverInterval => {
                write!(f, "time-table-over-interval")
            }
            CumulativePropagationMethod::TimeTableOverIntervalIncremental => {
                write!(f, "time-table-over-interval-incremental")
            }
            CumulativePropagationMethod::TimeTableOverIntervalIncrementalSynchronised => {
                write!(f, "time-table-over-interval-incremental-synchronised")
            }
        }
    }
}
