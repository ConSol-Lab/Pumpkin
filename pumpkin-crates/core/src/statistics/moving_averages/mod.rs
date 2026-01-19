pub(crate) mod cumulative_moving_average;
pub(crate) mod moving_average;
pub(crate) mod windowed_moving_average;

pub use cumulative_moving_average::CumulativeMovingAverage;
pub use moving_average::MovingAverage;
pub(crate) use windowed_moving_average::WindowedMovingAverage;
