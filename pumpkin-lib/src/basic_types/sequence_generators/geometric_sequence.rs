use super::SequenceGenerator;

/// Given constants 'a' and 'm', the i-th element f(i) in a geometric sequence is computed as:
///  f(i) = f(i-1) * m
///  f(0) = a
/// When 'm' is not an integer, the above formula is _not_ the same as f(i) = a * m^i since
/// intermediate values will be rounded down
/// 
/// Note that overflows are not taken into account
#[derive(Debug, Copy, Clone)]
pub(crate) struct GeometricSequence {
    current_value: i64,
    multiplication_factor: f64,
}

impl GeometricSequence {
    pub(crate) fn new(starting_value: i64, multiplication_factor: f64) -> GeometricSequence {
        GeometricSequence {
            current_value: starting_value,
            multiplication_factor,
        }
    }
}

impl SequenceGenerator for GeometricSequence {
    fn next(&mut self) -> i64 {
        let next_value = self.current_value;
        self.current_value = (self.current_value as f64 * self.multiplication_factor) as i64;
        next_value
    }
}

#[cfg(test)]
mod tests {
    use super::GeometricSequence;
    use crate::basic_types::sequence_generators::SequenceGenerator;

    #[test]
    fn test_2_pow_n() {
        let mut geometric_sequence = GeometricSequence::new(1, 2.0);
        for i in 0..63 {
            assert!(geometric_sequence.next() == 1_i64 << i);
        }
    }

    #[test]
    fn test_multiply_50_procent() {
        let mut geometric_sequence = GeometricSequence::new(100, 1.50);
        assert!(geometric_sequence.next() == 100);
        assert!(geometric_sequence.next() == 150);
        assert!(geometric_sequence.next() == 225);
        assert!(geometric_sequence.next() == 337);
        assert!(geometric_sequence.next() == 505);
        assert!(geometric_sequence.next() == 757);
        assert!(geometric_sequence.next() == 1135);
        assert!(geometric_sequence.next() == 1702);
        assert!(geometric_sequence.next() == 2553);
        assert!(geometric_sequence.next() == 3829);
        assert!(geometric_sequence.next() == 5743);
    }
}
