//! The time-table rule of the cumulative constraint.

mod inference;
#[cfg(test)]
mod tests;

#[derive(Clone, Debug)]
pub struct TimeTableChecker<Var> {
    pub tasks: Box<[CheckerTask<Var>]>,
    pub capacity: i32,
}

#[derive(Clone, Debug)]
pub struct CheckerTask<Var> {
    pub start_time: Var,
    pub resource_usage: i32,
    pub processing_time: i32,
}
