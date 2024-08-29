use super::Direction;
use super::TieBreaker;

/// A tie-breaker which simply selects the first variable that it receives with the "best" value
/// according to the provided [`Direction`].
///
///  For example, if the provided direction is [`Direction::Minimum`] and there are two variables
/// `x1` with value 5 and `x2` with value 5, if the tie-breaker first receives `x2` and then `x1`
/// then it will return `x2` because it was the first variable with the minimum value (of 5 in this
/// example) which was provided.
#[derive(Debug)]
pub struct InOrderTieBreaker<Var, Value> {
    /// The selected variable, could be [None] if no variable has been considered yet
    selected_variable: Option<Var>,
    /// The selected value, could be [None] if no variable has been considered yet
    selected_value: Option<Value>,
    /// Whether the tie-breaker should find the variable with the maximum or minimum value
    direction: Direction,
}

impl<Var, Value> InOrderTieBreaker<Var, Value> {
    pub fn new(direction: Direction) -> Self {
        Self {
            selected_variable: None,
            selected_value: None,
            direction,
        }
    }

    fn reset(&mut self) {
        self.selected_variable = None;
        self.selected_value = None;
    }
}

impl<Var: Copy, Value: PartialOrd> TieBreaker<Var, Value> for InOrderTieBreaker<Var, Value> {
    fn consider(&mut self, variable: Var, value: Value) {
        if let Some(selected_value) = self.selected_value.as_mut() {
            // We already have a stored variable and value, check whether it needs to be updated or
            // compared
            match self.direction {
                Direction::Maximum => {
                    // The current value is larger than the selected one, reset to this
                    // variable/value
                    if value > *selected_value {
                        self.selected_variable = Some(variable);
                        self.selected_value = Some(value);
                    }
                }
                Direction::Minimum => {
                    // The current value is larger than the selected one, reset to this
                    // variable/value
                    if value < *selected_value {
                        self.selected_variable = Some(variable);
                        self.selected_value = Some(value);
                    }
                }
            }
        } else {
            self.selected_variable = Some(variable);
            self.selected_value = Some(value);
        }
    }

    fn select(&mut self) -> Option<Var> {
        let selected = self.selected_variable;
        self.reset();
        selected
    }

    fn get_direction(&self) -> Direction {
        self.direction
    }
}

#[cfg(test)]
mod tests {
    use super::InOrderTieBreaker;
    use crate::branching::Direction;
    use crate::branching::TieBreaker;
    use crate::engine::variables::DomainId;

    #[test]
    fn test_selection_first_value() {
        let mut breaker = InOrderTieBreaker::new(Direction::Minimum);

        breaker.consider(DomainId::new(0), 10);
        breaker.consider(DomainId::new(1), 10);
        breaker.consider(DomainId::new(2), 10);

        let selected = breaker.select();
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), DomainId::new(0));
    }

    #[test]
    fn test_selection_picks_lowest_value() {
        let mut breaker = InOrderTieBreaker::new(Direction::Minimum);

        breaker.consider(DomainId::new(0), 10);
        breaker.consider(DomainId::new(1), 5);
        breaker.consider(DomainId::new(2), 10);

        let selected = breaker.select();
        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), DomainId::new(1));
    }
}
