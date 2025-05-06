#[derive(Debug, Clone)]
pub enum Constraint<VariableKey> {
    LinearInequality {
        linear_expr: Vec<(VariableKey, i32)>,
        lower_bound: Option<i32>,
        upper_bound: Option<i32>,
    },
    AllDifferent(Vec<VariableKey>),
}

impl<VariableKey: Clone> Constraint<VariableKey> {
    pub fn less_than_or_equals(linear_expr: Vec<(VariableKey, i32)>, upper_bound: i32) -> Self {
        Self::LinearInequality {
            linear_expr,
            lower_bound: None,
            upper_bound: Some(upper_bound),
        }
    }

    pub fn greater_than_or_equals(linear_expr: Vec<(VariableKey, i32)>, lower_bound: i32) -> Self {
        Self::LinearInequality {
            linear_expr,
            lower_bound: Some(lower_bound),
            upper_bound: None,
        }
    }

    pub fn equals(linear_expr: Vec<(VariableKey, i32)>, rhs: i32) -> Self {
        Self::LinearInequality {
            linear_expr,
            lower_bound: Some(rhs),
            upper_bound: Some(rhs),
        }
    }

    pub fn double_inequality(
        linear_expr: Vec<(VariableKey, i32)>,
        lower_bound: i32,
        upper_bound: i32,
    ) -> Self {
        Self::LinearInequality {
            linear_expr,
            lower_bound: Some(lower_bound),
            upper_bound: Some(upper_bound),
        }
    }

    pub fn all_different(vars: Vec<VariableKey>) -> Self {
        Self::AllDifferent(vars)
    }

    pub fn variables(&self) -> Box<dyn Iterator<Item = &VariableKey> + '_> {
        match self {
            Constraint::LinearInequality { linear_expr, .. } => {
                Box::new(linear_expr.iter().map(|(var, _)| var))
            }
            Constraint::AllDifferent(vars) => Box::new(vars.iter()),
        }
    }
}
