use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::Range;

use rand::Rng;
use rand::SeedableRng;

#[cfg(doc)]
use crate::branching::InDomainRandom;
#[cfg(doc)]
use crate::branching::SelectionContext;
use crate::pumpkin_assert_moderate;
#[cfg(doc)]
use crate::Solver;

/// A trait for generating random values; an example of where this is used is in the
/// [`InDomainRandom`] value selector where it is used to determine which value in the domain to
/// select.
///
/// At the moment, the randomness in the solver is controlled by the
/// [`Solver`] and the random number generator is by this structure to the
/// [`SelectionContext`].
///
/// # Testing
/// We have also created an implementation of this trait which takes as input a list of `usize`s and
/// `bool`s and returns them in that order. This allows the user to define deterministic test-cases
/// while the implementation makes use of an implementation of the [`Random`] trait.
pub trait Random: Debug {
    /// Generates a bool with probability `probability` of being true. It should hold that
    /// `probability ∈ [0, 1]`, this method will panic if this is not the case.
    ///
    /// # Example
    /// This example will show how to use a concrete implementation of [`SeedableRng`] to implement
    /// a fair coin toss (e.g. a [bernoulli trial](https://en.wikipedia.org/wiki/Bernoulli_trial) with probability 0.5).
    /// ```rust
    /// # use rand::rngs::SmallRng;
    /// # use rand::SeedableRng;
    /// # use pumpkin_lib::Random;
    /// // First we create our random object
    /// let mut rng = SmallRng::seed_from_u64(42);
    /// // Then we flip a coin with probability 0.5
    /// let coin_flip_outcome = rng.generate_bool(0.5);
    /// // A sanity check to ensure that the method has not panicked
    /// assert!(coin_flip_outcome || !coin_flip_outcome);
    ///
    /// // If we provide a probability outside of the expected range then the method is expected to panic!
    /// let result = std::panic::catch_unwind(|| rng.clone().generate_bool(120.0));
    /// assert!(result.is_err());
    /// ```
    fn generate_bool(&mut self, probability: f64) -> bool;

    /// Generates a random usize in the provided range with equal probability; this can be seen as
    /// sampling from a uniform distribution in the range `[range.start, range.end)`
    ///
    /// # Example
    /// This example will show how to use a concrete implementation of [`SeedableRng`] to implement
    /// selecting a random element from a list.
    /// ```rust
    /// # use rand::rngs::SmallRng;
    /// # use rand::SeedableRng;
    /// # use pumpkin_lib::Random;
    /// // First we create our random object
    /// let mut rng = SmallRng::seed_from_u64(42);
    /// // Then we create the elements to select from
    /// let elements = vec!["This", "is", "a", "test"];
    /// // Finally we generate a random number in the range [0, |elements|)
    /// let selected_index = rng.generate_usize_in_range(0..elements.len());
    /// assert!(selected_index >= 0 && selected_index < elements.len());
    /// ```
    fn generate_usize_in_range(&mut self, range: Range<usize>) -> usize;
}

// We provide a blanket implementation of the trait for any type which implements `SeedableRng`,
// `Rng` and `Debug` to ensure that we can use any "regular" random generator where we expect an
// implementation of Random.
impl<T> Random for T
where
    T: SeedableRng + Rng + Debug,
{
    fn generate_bool(&mut self, probability: f64) -> bool {
        pumpkin_assert_moderate!(
            !matches!(probability.partial_cmp(&0.0), Some(Ordering::Less))
                && !matches!(probability.partial_cmp(&1.0), Some(Ordering::Greater)),
            "It should hold that 0.0 <= {probability} <= 1.0"
        );
        self.gen_bool(probability)
    }

    fn generate_usize_in_range(&mut self, range: Range<usize>) -> usize {
        self.gen_range(range)
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::cmp::Ordering;
    use std::ops::Range;

    use super::Random;
    use crate::pumpkin_assert_simple;

    /// A test "random" generator which takes as input a list of elements of [`usize`] and [`bool`]
    /// and returns them in order. If more values are attempted to be generated than are provided
    /// then this will result in panicking.
    #[derive(Debug, Default)]
    pub(crate) struct TestRandom {
        pub(crate) usizes: Vec<usize>,
        pub(crate) bools: Vec<bool>,
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

        fn generate_usize_in_range(&mut self, range: Range<usize>) -> usize {
            let selected = self.usizes.remove(0);
            pumpkin_assert_simple!(
                range.contains(&selected),
                "The selected element by `TestRandom` ({selected}) is not in the provided range ({range:?}) and thus should not be returned, please ensure that your test cases are correctly defined"
            );
            selected
        }
    }
}
