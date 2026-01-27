use std::fmt::Debug;
use std::fmt::Display;

use num::cast::AsPrimitive;
use num::traits::NumAssign;

use super::MovingAverage;

#[derive(Default, Debug, Copy, Clone)]
pub struct CumulativeMovingAverage<Term> {
    sum: Term,
    num_terms: u64,
}

impl<Term> Display for CumulativeMovingAverage<Term>
where
    Term: Debug + NumAssign + AsPrimitive<f64>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl<Term> MovingAverage<Term> for CumulativeMovingAverage<Term>
where
    Term: Debug + NumAssign + AsPrimitive<f64>,
{
    fn add_term(&mut self, new_term: Term) {
        self.sum += new_term;
        self.num_terms += 1
    }

    fn value(&self) -> f64 {
        if self.num_terms > 0 {
            self.sum.as_() / (self.num_terms as f64)
        } else {
            0.0
        }
    }

    fn adapt(&mut self, _interval_length: u64) {
        // do nothing
    }
}

#[cfg(test)]
mod tests {
    use super::CumulativeMovingAverage;
    use crate::statistics::moving_averages::MovingAverage;

    #[test]
    fn test_constant_value() {
        let constant_value = 100;
        let mut constant_average = CumulativeMovingAverage::default();
        for _i in 0..1000 {
            constant_average.add_term(constant_value);
            assert!(constant_average.value() == constant_value as f64);
        }
    }

    #[test]
    fn test_empty() {
        let empty_sum: CumulativeMovingAverage<u64> = CumulativeMovingAverage::default();
        assert!(empty_sum.value() == 0.0);
    }

    #[test]
    fn test_simple1() {
        let mut constant_average = CumulativeMovingAverage::default();
        constant_average.add_term(10);
        assert!(constant_average.value() == 10.0);
        constant_average.add_term(20);
        assert!(constant_average.value() == 15.0);
        constant_average.add_term(30);
        assert!(constant_average.value() == 20.0);
    }
}
