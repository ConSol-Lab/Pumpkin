use std::fmt::Display;

use super::MovingAverage;

#[derive(Default, Debug, Copy, Clone)]
pub(crate) struct CumulativeMovingAverage {
    sum: u64,
    num_terms: u64,
}

impl Display for CumulativeMovingAverage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl MovingAverage for CumulativeMovingAverage {
    fn add_term(&mut self, new_term: u64) {
        self.sum += new_term;
        self.num_terms += 1
    }

    fn value(&self) -> f64 {
        if self.num_terms > 0 {
            (self.sum as f64) / (self.num_terms as f64)
        } else {
            0.0
        }
    }

    fn adapt(&mut self, _interval_length: u64) {
        // do nothing
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub(crate) struct CumulativeMovingAverageFloat {
    sum: f64,
    num_terms: u64,
}

impl Display for CumulativeMovingAverageFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl MovingAverage for CumulativeMovingAverageFloat {
    fn add_term_float(&mut self, new_term: f64) {
        self.sum += new_term;
        self.num_terms += 1
    }

    fn value(&self) -> f64 {
        if self.num_terms > 0 {
            self.sum / (self.num_terms as f64)
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
    use crate::basic_types::moving_averages::MovingAverage;

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
        let empty_sum = CumulativeMovingAverage::default();
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
