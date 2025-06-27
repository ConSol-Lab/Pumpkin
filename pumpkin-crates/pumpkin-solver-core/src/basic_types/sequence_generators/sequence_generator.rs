use std::fmt::Debug;

pub(crate) trait SequenceGenerator: Debug {
    fn next(&mut self) -> i64;
}
