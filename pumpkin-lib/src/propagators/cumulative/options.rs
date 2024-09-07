use std::fmt::Display;

use super::CumulativeExplanationType;

#[derive(Debug, Copy, Clone, Default)]
pub struct CumulativeOptions {
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub allow_holes_in_domain: bool,
    /// The type of explanation which is used by the cumulative to explain propagations and
    /// conflicts.
    pub explanation_type: CumulativeExplanationType,
    /// The propagation method which is used for the cumulative constraints; currently all of them
    /// are variations of time-tabling. The default is incremental time-tabling reasoning over
    /// intervals.
    pub propagation_method: CumulativePropagationMethod,
    /// Determines whether a sequence of profiles is generated when explaining a propagation.
    pub generate_sequence: bool,
}

#[derive(Debug, Default, Clone, Copy)]
pub enum CumulativePropagationMethod {
    TimeTablePerPoint,
    TimeTablePerPointIncremental,
    TimeTableOverInterval,
    #[default]
    TimeTableOverIntervalIncremental,
}

impl Display for CumulativePropagationMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CumulativePropagationMethod::TimeTablePerPoint => write!(f, "TimeTablePerPoint"),
            CumulativePropagationMethod::TimeTablePerPointIncremental => {
                write!(f, "TimeTablePerPointIncremental")
            }
            CumulativePropagationMethod::TimeTableOverInterval => {
                write!(f, "TimeTableOverInterval")
            }
            CumulativePropagationMethod::TimeTableOverIntervalIncremental => {
                write!(f, "TimeTableOverIntervalIncremental")
            }
        }
    }
}
