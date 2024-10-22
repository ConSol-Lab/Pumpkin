pub(crate) mod cumulative_moving_average;
pub(crate) mod moving_average;
pub(crate) mod windowed_moving_average;

pub(crate) use cumulative_moving_average::CumulativeMovingAverage;
pub(crate) use moving_average::MovingAverage;
pub(crate) use windowed_moving_average::WindowedMovingAverage;
