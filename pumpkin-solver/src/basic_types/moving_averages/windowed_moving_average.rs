use std::collections::VecDeque;
use std::fmt::Debug;

use num::cast::AsPrimitive;
use num::traits::NumAssign;

use super::MovingAverage;
use crate::pumpkin_assert_simple;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub(crate) struct WindowedMovingAverage<Term> {
    window_size: u64,
    windowed_sum: Term,
    values_in_window: VecDeque<Term>,
}

impl<Term: Default> WindowedMovingAverage<Term> {
    pub(crate) fn new(window_size: u64) -> WindowedMovingAverage<Term> {
        pumpkin_assert_simple!(window_size > 0);
        WindowedMovingAverage {
            window_size,
            windowed_sum: Term::default(),
            values_in_window: VecDeque::with_capacity(window_size as usize),
        }
    }
}

impl<Term> MovingAverage<Term> for WindowedMovingAverage<Term>
where
    Term: Debug + NumAssign + AsPrimitive<f64>,
{
    fn add_term(&mut self, new_term: Term) {
        pumpkin_assert_simple!(self.values_in_window.len() <= self.window_size as usize);

        // if the window is full, then remove an element to make room for the new term
        if self.values_in_window.len() == self.window_size as usize {
            self.windowed_sum -= self.values_in_window.pop_front().unwrap();
        }

        self.windowed_sum += new_term;
        self.values_in_window.push_back(new_term);
    }

    fn value(&self) -> f64 {
        if !self.values_in_window.is_empty() {
            self.windowed_sum.as_() / (self.values_in_window.len() as f64)
        } else {
            0.0
        }
    }

    fn adapt(&mut self, interval_length: u64) {
        pumpkin_assert_simple!(interval_length > 0);

        match interval_length.cmp(&self.window_size) {
            std::cmp::Ordering::Less => {
                // remove excess values
                let num_removals = self.window_size - interval_length;
                for _i in 0..num_removals {
                    self.windowed_sum -= self.values_in_window.pop_front().unwrap();
                }
                self.window_size = interval_length;
            }
            std::cmp::Ordering::Greater => {
                // allow for more values by increasing the window size
                self.window_size = interval_length
            }
            std::cmp::Ordering::Equal => { /*do nothing*/ }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_value() {
        let constant_value = 100;
        let mut constant_average = WindowedMovingAverage::new(5);
        for _i in 0..1000 {
            constant_average.add_term(constant_value);
            assert!(constant_average.value() == constant_value as f64);
        }
    }

    #[test]
    fn test_empty() {
        let empty_sum: WindowedMovingAverage<u64> = WindowedMovingAverage::new(10);
        assert!(empty_sum.value() == 0.0);
    }

    #[test]
    fn test_simple1() {
        let mut constant_average = WindowedMovingAverage::new(2);
        constant_average.add_term(10);
        assert!(constant_average.value() == 10.0);
        constant_average.add_term(30);
        assert!(constant_average.value() == 20.0);
        constant_average.add_term(30);
        assert!(constant_average.value() == 30.0);
        constant_average.add_term(10);
        assert!(constant_average.value() == 20.0);
        constant_average.add_term(90);
        assert!(constant_average.value() == 50.0);
    }
}
