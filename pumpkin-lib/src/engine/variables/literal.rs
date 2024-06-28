use crate::basic_types::StorageKey;
use crate::engine::variables::PropositionalVariable;
use crate::pumpkin_assert_moderate;

/// A boolean variable in the solver; represents a [`PropositionalVariable`] but with a certain
/// polarity (i.e. it is either the positive [`PropositionalVariable`] or its negation).
#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal {
    code: u32,
}

impl Literal {
    pub fn new(propositional_variable: PropositionalVariable, is_positive: bool) -> Literal {
        Literal {
            code: propositional_variable.index() as u32 * 2 + (is_positive as u32),
        }
    }

    pub fn is_positive(&self) -> bool {
        (self.code & 1) == 1
    }

    pub fn is_negative(&self) -> bool {
        (self.code & 1) == 0
    }

    pub fn get_propositional_variable(&self) -> PropositionalVariable {
        PropositionalVariable::new(self.code / 2)
    }

    pub fn to_u32(self) -> u32 {
        self.code
    }

    pub fn u32_to_literal(literal_code: u32) -> Literal {
        let variable_index = literal_code / 2;
        let code = variable_index * 2 + ((literal_code & 1) == 1) as u32;
        pumpkin_assert_moderate!(Literal { code }.to_u32() == literal_code);
        Literal { code }
    }
}

impl std::ops::Not for Literal {
    type Output = Literal;
    fn not(self) -> Literal {
        Literal::new(self.get_propositional_variable(), !self.is_positive())
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_negative() {
            write!(f, "~{}", self.get_propositional_variable())
        } else {
            write!(f, "{}", self.get_propositional_variable())
        }
    }
}

impl StorageKey for Literal {
    fn index(&self) -> usize {
        self.to_u32() as usize
    }

    fn create_from_index(index: usize) -> Self {
        Literal { code: index as u32 }
    }
}
