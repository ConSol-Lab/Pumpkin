use super::TerminationCondition;

/// A [`TerminationCondition`] which never triggers. The solver can search forever.
#[derive(Clone, Copy, Debug)]
pub struct Indefinite;

impl TerminationCondition for Indefinite {
    fn should_stop(&mut self) -> bool {
        false
    }
}
