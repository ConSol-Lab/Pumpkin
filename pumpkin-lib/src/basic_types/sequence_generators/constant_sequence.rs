use super::SequenceGeneratorInterface;

//A sequence that generates the same value
pub struct ConstantSequence {
    constant_value: i64,
}

impl ConstantSequence {
    pub fn new(constant_value: i64) -> ConstantSequence {
        ConstantSequence { constant_value }
    }
}

impl SequenceGeneratorInterface for ConstantSequence {
    fn next(&mut self) -> i64 {
        self.constant_value
    }
}

#[cfg(test)]
mod tests {
    use crate::basic_types::sequence_generators::SequenceGeneratorInterface;

    use super::ConstantSequence;

    #[test]
    fn test_basic() {
        let constant_value = 100;
        let mut constant_sequence = ConstantSequence::new(constant_value);
        for _i in 0..1000 {
            assert!(constant_sequence.next() == constant_value);
        }
    }
}