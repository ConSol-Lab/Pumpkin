use std::fmt::Debug;

pub trait SequenceGenerator: Debug {
    fn next(&mut self) -> i64;
}
