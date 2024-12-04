use super::Direction;
use super::TieBreaker;
use crate::basic_types::Random;

/// A tie breaker which selects the variable with the "best" value (according to the [`Direction`]),
/// if there is a tie then it will select any of the variables part of this tie with equal
/// probability.
///
/// The random selection proceeds as follows:
/// - If no variable has been considered yet then this is the one which is currently considered to
///   be selected
/// - If a variable has previously been considered then we can split into 3 cases:
///     - If the direction is [`Direction::Maximum`] and the value of the newly provided variable is
///       stricly bigger than that of the currently selected variable then we update the currently
///       selected variable
///     - If the direction is [`Direction::Minimum`] and the value of the newly provided variable is
///       stricly smaller than that of the currently selected variable then we update the currently
///       selected variable
///     - If the values are equal then we randomly select the newly considered variable with
///       probability `1 / num_previously_seen_variables` where `num_previously_seen_variables` is
///       the number of variables which have been previously considered with the same value
pub struct RandomTieBreaker<Var, Value> {
    /// The selected variable, could be [None] if no variable has been considered yet
    selected_variable: Option<Var>,
    /// The selected value, could be [None] if no variable has been considered yet
    selected_value: Option<Value>,
    /// The [SeedableRng] which is used for randomly selecting the variable
    rng: Box<dyn Random>,
    /// The number of variables with the current
    /// [`selected_value`][RandomTieBreaker::selected_value]
    num_variables_considered: usize,
    /// Whether the tie-breaker should find the variable with the maximum or minimum value
    direction: Direction,
}

impl<Var, Value> std::fmt::Debug for RandomTieBreaker<Var, Value> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RandomTieBreaker").finish()
    }
}

impl<Var, Value> RandomTieBreaker<Var, Value> {
    // Currently the struct is not used
    pub fn new(direction: Direction, rng: Box<dyn Random>) -> Self {
        Self {
            selected_variable: None,
            selected_value: None,
            rng,
            num_variables_considered: 0,
            direction,
        }
    }

    fn reset(&mut self) {
        self.selected_variable = None;
        self.selected_value = None;
    }
}

impl<Var: Copy, Value: PartialOrd> TieBreaker<Var, Value> for RandomTieBreaker<Var, Value> {
    fn consider(&mut self, variable: Var, value: Value) {
        if let Some(selected_var) = self.selected_variable.as_mut() {
            // We already have a stored variable and value, check whether it needs to be updated or
            // compared
            let selected_value = self
                .selected_value
                .as_ref()
                .expect("The random tie breaker selected a variable but not a value...");
            match self.direction {
                Direction::Maximum => {
                    // The current value is larger than the selected one, reset to this
                    // variable/value
                    if value > *selected_value {
                        self.num_variables_considered = 1;
                        self.selected_variable = Some(variable);
                        self.selected_value = Some(value);
                        return;
                    }
                }
                Direction::Minimum => {
                    // The current value is larger than the selected one, reset to this
                    // variable/value
                    if value < *selected_value {
                        self.num_variables_considered = 1;
                        self.selected_variable = Some(variable);
                        self.selected_value = Some(value);
                        return;
                    }
                }
            }
            if value == *selected_value {
                // The current value and the provided one are equal, we randomly select which one to
                // choose
                self.num_variables_considered += 1;
                if self
                    .rng
                    .generate_bool(1.0 / self.num_variables_considered as f64)
                {
                    *selected_var = variable;
                    self.selected_value = Some(value);
                }
            }
        } else {
            self.num_variables_considered = 1;
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
    use super::RandomTieBreaker;
    use crate::basic_types::tests::TestRandom;
    use crate::branching::tie_breaking::random_tie_breaker::Direction;
    use crate::branching::tie_breaking::TieBreaker;

    #[test]
    fn test_selection_new_value() {
        let rng = TestRandom::default();
        let mut breaker: RandomTieBreaker<i32, i32> =
            RandomTieBreaker::new(Direction::Minimum, Box::new(rng));

        assert!(breaker.select().is_none());

        breaker.consider(0, 1);

        let selected = breaker.select();

        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), 0);
    }

    #[test]
    fn test_selection_between_values_chooses_maximum() {
        let rng = TestRandom::default();
        let mut breaker: RandomTieBreaker<i32, i32> =
            RandomTieBreaker::new(Direction::Maximum, Box::new(rng));

        breaker.consider(0, 5);
        breaker.consider(1, 10);

        let selected = breaker.select();

        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), 1);
    }

    #[test]
    fn test_selection_between_values_chooses_minimum() {
        let rng = TestRandom::default();
        let mut breaker: RandomTieBreaker<i32, i32> =
            RandomTieBreaker::new(Direction::Minimum, Box::new(rng));

        breaker.consider(0, 5);
        breaker.consider(1, 10);

        let selected = breaker.select();

        assert!(selected.is_some());
        assert_eq!(selected.unwrap(), 0);
    }

    #[test]
    fn test_selection_between_values_chooses_random_with_seed_second() {
        let rng = TestRandom {
            bools: vec![true],
            ..Default::default()
        };
        let mut breaker: RandomTieBreaker<i32, i32> =
            RandomTieBreaker::new(Direction::Maximum, Box::new(rng));

        breaker.consider(0, 5);
        breaker.consider(1, 5);

        let selected = breaker.select();

        assert!(selected.is_some());
        // The seeding will result in the second being selected
        assert_eq!(selected.unwrap(), 1);
    }

    #[test]
    fn test_selection_between_values_chooses_random_with_seed_first() {
        let rng = TestRandom {
            bools: vec![false],
            ..Default::default()
        };
        let mut breaker: RandomTieBreaker<i32, i32> =
            RandomTieBreaker::new(Direction::Maximum, Box::new(rng));

        breaker.consider(0, 5);
        breaker.consider(1, 5);

        let selected = breaker.select();

        assert!(selected.is_some());
        // The seeding will result in the first being selected
        assert_eq!(selected.unwrap(), 0);
    }
}
