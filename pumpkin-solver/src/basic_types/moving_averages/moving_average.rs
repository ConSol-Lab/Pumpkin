use std::fmt::Debug;

pub(crate) trait MovingAverage<Term>: Debug {
    fn add_term(&mut self, new_term: Term);

    /// Returns the moving average value; in case there are no terms, the convention is to return 0
    fn value(&self) -> f64;

    /// Adapts the internal data structures to take into account the given interval length; this
    /// makes sense for moving averages that consider the k previous points, e.g., windowed moving
    /// average
    fn adapt(&mut self, interval_length: u64);
}

