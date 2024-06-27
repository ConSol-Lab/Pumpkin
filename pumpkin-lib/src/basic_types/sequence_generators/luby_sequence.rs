use super::SequenceGenerator;

// The Luby sequence is a recursive sequence of the form:
// 1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8, 1, 1, 2....
//  The above sequence is multiplied with a given constant 'base_value'
// Generating the next element is computed in constant time using Knuth's 'reluctant doubling'
// formula Note that overflows are not taken into account
#[derive(Debug, Copy, Clone)]
pub(crate) struct LubySequence {
    u: i64,
    v: i64,
    base_value: i64,
}

impl LubySequence {
    pub(crate) fn new(base_value: i64) -> LubySequence {
        LubySequence {
            u: 1,
            v: 1,
            base_value,
        }
    }
}

impl SequenceGenerator for LubySequence {
    fn next(&mut self) -> i64 {
        // The implementation follows Donald Knuth's 'reluctant doubling' formula
        let next_value = self.v;
        if (self.u & (-self.u)) == self.v {
            self.u += 1;
            self.v = 1;
        } else {
            self.v *= 2;
        }
        next_value * self.base_value
    }
}

#[cfg(test)]
mod tests {
    use super::LubySequence;
    use crate::basic_types::sequence_generators::SequenceGenerator;

    #[test]
    fn test_base_1() {
        // 1, 1, 2, 1, 1, 2, 4, 1, 1, 2, 1, 1, 2, 4, 8, 1, 1, 2....
        let mut luby_sequence = LubySequence::new(1);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 2);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 2);
        assert!(luby_sequence.next() == 4);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 2);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 2);
        assert!(luby_sequence.next() == 4);
        assert!(luby_sequence.next() == 8);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 1);
        assert!(luby_sequence.next() == 2);
    }

    #[test]
    fn test_base_100() {
        // 100, 100, 200, 100, 100, 200, 400, 100, 100, 200, 100, 100, 200, 400, 800, 100, 100,
        // 200....
        let mut luby_sequence = LubySequence::new(100);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 200);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 200);
        assert!(luby_sequence.next() == 400);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 200);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 200);
        assert!(luby_sequence.next() == 400);
        assert!(luby_sequence.next() == 800);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 100);
        assert!(luby_sequence.next() == 200);
    }

    fn luby_compute_recursively(i: usize) -> usize {
        let k = (i + 1).ilog2();
        if (i + 1).is_power_of_two() {
            1 << (k - 1)
        } else {
            luby_compute_recursively(i + 1 - (1 << k))
        }
    }

    #[test]
    fn test_base_1_long() {
        let mut luby_sequence = LubySequence::new(1);
        for i in 1..100000 {
            assert!(luby_sequence.next() == luby_compute_recursively(i) as i64);
        }
    }

    #[test]
    fn test_base_50_long() {
        let mut luby_sequence = LubySequence::new(50);
        for i in 1..100000 {
            assert!(luby_sequence.next() == (luby_compute_recursively(i) * 50) as i64);
        }
    }
}
