// This is an implementation of the exponential moving average as described in the paper "Evaluating
// CDCL Restart Schemes" by Biere and Fröhlich (page 14). The idea is to include a smoothing factor
// to eliminate the strong bias of the first term.
//
// The bias is taken into account as described in the paper, i.e. X_smooth is used instead of X, and
// X_smooth is gradually increased until it reaches X It uses a fixed-point arithmetic rather than
// floating points and includes the bias-smoothing technique.
//
// The formula represented is: G_{i+1} = g_{i}*2^{32-X} + (1 - 2^{-X})*G_{i}, where g_i is the i-th
// term, G_i is the value after i terms, and X is the parameter Note that, essentially, the
// upper/lower 32 bits are used as the value before/after the decimal point It includes a smoothing
// factor to eliminate the strong bias of the first term. This is done as described in the paper,
// i.e. X_smooth is used instead of X, and X_smooth is gradually increased until it reaches X

// todo reevaluate this implementation

use std::cmp::min;

use bitfield::BitRange;

use super::MovingAverage;

#[derive(Default, Debug, Copy, Clone)]
pub(crate) struct ExponentialMovingAverage {
    x_current: u64,
    current_value: u64,
}
impl MovingAverage for ExponentialMovingAverage {
    // adds a term to the exponential moving average according to the formula above, including
    // smoothing. The input term is expected to be in a proper integer, e.g., not given in
    // fixed-point arithmetic form.
    fn add_term(&mut self, new_term: u64) {
        // to take into account binary arithmetic the formula is computed as:
        // G_{i+1} = g_{i}*2^{32-X} + (G_{i} - 2^{-X}*G_{i})
        let first_part = new_term << (32 - self.x_current);
        let third_part = self.current_value >> self.x_current;
        self.current_value = first_part + (self.current_value - third_part);

        // considering increasing the X_current if its not yet at its target value, this is part of
        // smoothing
        self.x_current = min(self.x_current + 1, 32);
    }

    fn value(&self) -> f64 {
        // recall that the most significant 32 bits hold the integer part, whereas the least
        // significant 32 bits hold the decimal part
        let integer_part = <u64 as BitRange<u64>>::bit_range(&self.current_value, 63, 32) as f64;

        let decimal_bits = <u64 as BitRange<u64>>::bit_range(&self.current_value, 31, 0) as f64;
        let decimal_part = decimal_bits / (u32::MAX as f64);

        integer_part + decimal_part
    }

    fn adapt(&mut self, _interval_length: u64) {
        todo!()
    }
}
