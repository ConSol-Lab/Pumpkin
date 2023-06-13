use crate::pumpkin_assert_moderate;

use super::PropositionalVariable;

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Literal {
    code: u32,
}

impl Literal {
    pub fn new(propositional_variable: PropositionalVariable, is_positive: bool) -> Literal {
        Literal {
            code: propositional_variable.index() * 2 + (is_positive as u32),
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

impl<T> std::ops::Index<Literal> for Vec<T> {
    type Output = T;
    fn index(&self, index_literal: Literal) -> &T {
        self.index(index_literal.to_u32() as usize)
    }
}

impl<T> std::ops::IndexMut<Literal> for Vec<T> {
    fn index_mut(&mut self, index_literal: Literal) -> &mut T {
        self.index_mut(index_literal.to_u32() as usize)
    }
}
