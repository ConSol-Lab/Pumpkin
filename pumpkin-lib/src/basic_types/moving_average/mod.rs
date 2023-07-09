pub mod cumulative_moving_average;
pub mod exponential_moving_average;
pub mod moving_average_interface;
pub mod windowed_moving_average;

pub use cumulative_moving_average::CumulativeMovingAverage;
pub use exponential_moving_average::ExponentialMovingAverage;
pub use moving_average_interface::MovingAverageInterface;
pub use windowed_moving_average::WindowedMovingAverage;
