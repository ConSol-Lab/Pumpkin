use std::fmt::Debug;
use std::ops::Range;

#[allow(
    clippy::disallowed_types,
    reason = "we implement our random generator using rand"
)]
use rand::Rng;
#[allow(
    clippy::disallowed_types,
    reason = "we implement our random generator using rand"
)]
use rand::RngExt;
#[allow(
    clippy::disallowed_types,
    reason = "we implement our random generator using rand"
)]
use rand::SeedableRng;

use crate::pumpkin_assert_moderate;

/// Abstraction for randomness, in order to swap out different source of randomness.
///
/// This is especially useful when testing, to control which variables are generated when random
/// values are required.
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
    /// # use pumpkin_core::Random;
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
    /// # use pumpkin_core::Random;
    /// // First we create our random object
    /// let mut rng = SmallRng::seed_from_u64(42);
    /// // Then we create the elements to select from
    /// let elements = vec!["This", "is", "a", "test"];
    /// // Finally we generate a random number in the range [0, |elements|)
    /// let selected_index = rng.generate_usize_in_range(0..elements.len());
    /// assert!(selected_index >= 0 && selected_index < elements.len());
    /// ```
    fn generate_usize_in_range(&mut self, range: Range<usize>) -> usize;

    /// Generates a random i32 in the provided range with equal probability; this can be seen as
    /// sampling from a uniform distribution in the range `[lb, ub]`
    fn generate_i32_in_range(&mut self, lb: i32, ub: i32) -> i32;

    /// Generate a random float in the range 0..1.
    fn generate_f64(&mut self) -> f64;

    /// Given a slice of weights, select the index with `weight` weighted probability compared to
    /// the other weights.
    fn get_weighted_choice(&mut self, weights: &[f64]) -> Option<usize>;
}

// We provide a blanket implementation of the trait for any type which implements `SeedableRng`,
// `Rng` and `Debug` to ensure that we can use any "regular" random generator where we expect an
// implementation of Random.
#[allow(
    clippy::disallowed_types,
    reason = "we implement our random generator using rand"
)]
impl<T> Random for T
where
    T: SeedableRng + Rng + Debug,
{
    fn generate_bool(&mut self, probability: f64) -> bool {
        pumpkin_assert_moderate!(
            (0.0..=1.0).contains(&probability),
            "It should hold that 0.0 <= {probability} <= 1.0"
        );

        self.random_bool(probability)
    }

    fn generate_usize_in_range(&mut self, range: Range<usize>) -> usize {
        self.random_range(range)
    }

    fn generate_i32_in_range(&mut self, lb: i32, ub: i32) -> i32 {
        self.random_range(lb..=ub)
    }

    fn generate_f64(&mut self) -> f64 {
        self.random_range(0.0..1.0)
    }

    fn get_weighted_choice(&mut self, weights: &[f64]) -> Option<usize> {
        // Taken from https://docs.rs/random_choice/latest/src/random_choice/lib.rs.html
        if weights.is_empty() {
            return None;
        }

        let sum = weights.iter().sum::<f64>();
        let spin = self.generate_f64() * sum;

        let mut i: usize = 0;
        let mut accumulated_weights = weights[0];

        while accumulated_weights < spin {
            i += 1;
            accumulated_weights += weights[i];
        }

        Some(i)
    }
}
