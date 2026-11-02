//! Testing utilities for the [`Random`] trait, used across this crate's test modules.

use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::Range;

use pumpkin_core::Random;
use pumpkin_core::asserts::pumpkin_assert_simple;

/// A test "random" generator which takes as input a list of elements of [`usize`] and [`bool`]
/// and returns them in order. If more values are attempted to be generated than are provided
/// then this will result in panicking.
#[derive(Debug)]
pub(crate) struct TestRandom {
    pub usizes: Vec<usize>,
    pub integers: Vec<i32>,
    pub bools: Vec<bool>,
    pub weighted_choice: fn(&[f64]) -> Option<usize>,
}

impl Default for TestRandom {
    fn default() -> Self {
        TestRandom {
            weighted_choice: |_| unimplemented!(),
            usizes: vec![],
            integers: vec![],
            bools: vec![],
        }
    }
}

impl Random for TestRandom {
    fn generate_bool(&mut self, probability: f64) -> bool {
        let selected = self.bools.remove(0);
        pumpkin_assert_simple!(
            if matches!(probability.partial_cmp(&1.0), Some(Ordering::Equal)) {
                selected
            } else if matches!(probability.partial_cmp(&0.0), Some(Ordering::Equal)) {
                !selected
            } else {
                true
            },
            "The probability is {probability} but the selected value is {selected}, this should not be possible, please ensure that your test cases are correctly defined"
        );
        selected
    }

    fn generate_i32_in_range(&mut self, lb: i32, ub: i32) -> i32 {
        let selected = self.integers.remove(0);
        let range = lb..ub;
        pumpkin_assert_simple!(
            range.contains(&selected),
            "The selected element by `TestRandom` ({selected}) is not in the provided range ({range:?}) and thus should not be returned, please ensure that your test cases are correctly defined"
        );
        selected
    }

    fn generate_usize_in_range(&mut self, range: Range<usize>) -> usize {
        let selected = self.usizes.remove(0);
        pumpkin_assert_simple!(
            range.contains(&selected),
            "The selected element by `TestRandom` ({selected}) is not in the provided range ({range:?}) and thus should not be returned, please ensure that your test cases are correctly defined"
        );
        selected
    }

    fn generate_f64(&mut self) -> f64 {
        unimplemented!()
    }

    fn get_weighted_choice(&mut self, weights: &[f64]) -> Option<usize> {
        (self.weighted_choice)(weights)
    }
}
