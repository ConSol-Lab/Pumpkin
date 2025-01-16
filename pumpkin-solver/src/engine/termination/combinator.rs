use super::TerminationCondition;

/// A [`TerminationCondition`] which triggers when one of two given [`TerminationCondition`]s
/// triggers.
#[derive(Clone, Copy, Debug)]
pub struct Combinator<T1, T2> {
    t1: T1,
    t2: T2,
}

impl<T1, T2> Combinator<T1, T2> {
    /// Combine two [`TerminationCondition`]s into one.
    pub fn new(t1: T1, t2: T2) -> Self {
        Combinator { t1, t2 }
    }
}

impl<T1: TerminationCondition, T2: TerminationCondition> TerminationCondition
    for Combinator<T1, T2>
{
    fn should_stop(&mut self) -> bool {
        self.t1.should_stop() || self.t2.should_stop()
    }

    fn decision_has_been_made(&mut self) {
        self.t1.decision_has_been_made();
        self.t2.decision_has_been_made();
    }
}
