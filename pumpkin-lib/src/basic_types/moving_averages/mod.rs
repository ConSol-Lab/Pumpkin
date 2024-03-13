pub mod cumulative_moving_average;
pub mod exponential_moving_average;
pub mod moving_average;
pub mod windowed_moving_average;

pub use cumulative_moving_average::CumulativeMovingAverage;
pub use exponential_moving_average::ExponentialMovingAverage;
pub use moving_average::MovingAverage;
pub use windowed_moving_average::WindowedMovingAverage;
