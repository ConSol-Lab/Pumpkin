//! [`Propagator`] for the Cumulative constraint; it
//! reasons over individual time-points instead of intervals. See [`TimeTablePerPointPropagator`]
//! for more information.
mod constructor;
mod propagator;
#[cfg(test)]
mod tests;

pub use propagator::*;
