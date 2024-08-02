use super::CumulativeExplanationType;

#[derive(Debug, Copy, Clone, Default)]
pub struct CumulativeOptions {
    /// Specifies whether it is allowed to create holes in the domain; if this parameter is set to
    /// false then it will only adjust the bounds when appropriate rather than removing values from
    /// the domain
    pub(crate) allow_holes_in_domain: bool,
    /// The type of explanation which is used by the cumulative to explain propagations and
    /// conflicts.
    pub(crate) explanation_type: CumulativeExplanationType,
    /// Determines whether a sequence of profiles is generated when explaining a propagation.
    pub(crate) generate_sequence: bool,
}

impl CumulativeOptions {
    pub fn new(
        allow_holes_in_domain: bool,
        explanation_type: CumulativeExplanationType,
        generate_sequence: bool,
    ) -> Self {
        Self {
            allow_holes_in_domain,
            explanation_type,
            generate_sequence,
        }
    }
}
