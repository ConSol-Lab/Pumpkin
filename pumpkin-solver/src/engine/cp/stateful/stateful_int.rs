use std::cell::RefCell;
use std::ops::Add;
use std::ops::Sub;
use std::rc::Rc;

use super::StateChange;
use crate::basic_types::Trail;

#[derive(Debug, Clone)]
pub struct StatefulInt {
    value: Rc<RefCell<i64>>,
}

impl StatefulInt {
    /// Constructed a new trailed i64 with an initial value.
    pub(crate) fn new(value: i64) -> StatefulInt {
        StatefulInt {
            value: Rc::new(RefCell::new(value)),
        }
    }

    /// Get the current value.
    pub(crate) fn read(&self) -> i64 {
        *(*self.value).borrow()
    }

    /// Update the value in a trail-aware manner.
    pub(crate) fn write(&mut self, value: i64, trail_writer: &mut Trail<StateChange>) {
        let old_value = self.read();
        if old_value == value {
            return;
        }
        let entry = StateChange {
            old_value,
            reference: Rc::clone(&self.value),
        };
        trail_writer.push(entry);
        *self.value.borrow_mut() = value;
    }

    pub(crate) fn add_assign(&mut self, value: i64, trail_writer: &mut Trail<StateChange>) {
        self.write(self.read() + value, trail_writer);
    }

    pub(crate) fn assign(&mut self, value: i64, trail_writer: &mut Trail<StateChange>) {
        self.write(value, trail_writer)
    }
}
impl Add<StatefulInt> for i64 {
    type Output = i64;
    fn add(self, rhs: StatefulInt) -> Self::Output {
        rhs + self
    }
}
impl Add<i64> for StatefulInt {
    type Output = i64;
    fn add(self, rhs: i64) -> Self::Output {
        self.read() + rhs
    }
}
impl<'a> Sub<&'a StatefulInt> for i64 {
    type Output = i64;
    fn sub(self, rhs: &'a StatefulInt) -> Self::Output {
        self - rhs.read()
    }
}
impl Sub<i64> for StatefulInt {
    type Output = i64;
    fn sub(self, rhs: i64) -> Self::Output {
        self.read() - rhs
    }
}
impl PartialEq<i64> for StatefulInt {
    fn eq(&self, other: &i64) -> bool {
        self.read() == *other
    }
}
impl PartialEq<StatefulInt> for i64 {
    fn eq(&self, other: &StatefulInt) -> bool {
        *self == other.read()
    }
}
impl PartialOrd<i64> for StatefulInt {
    fn partial_cmp(&self, other: &i64) -> Option<std::cmp::Ordering> {
        Some(self.read().cmp(other))
    }
}
