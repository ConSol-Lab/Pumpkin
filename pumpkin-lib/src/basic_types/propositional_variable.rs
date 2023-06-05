#[derive(Copy, Clone, Eq, PartialEq)]
pub struct PropositionalVariable {
    index: u32,
}

impl PropositionalVariable {
    pub fn new(index: u32) -> PropositionalVariable {
        PropositionalVariable { index }
    }

    pub fn index(&self) -> u32 {
        self.index
    }
}

impl std::fmt::Display for PropositionalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "p{}", self.index)
    }
}

impl<T> std::ops::Index<PropositionalVariable> for Vec<T> {
    type Output = T;
    fn index(&self, variable: PropositionalVariable) -> &T {
        self.index(variable.index() as usize)
    }
}

impl<T> std::ops::IndexMut<PropositionalVariable> for Vec<T> {
    fn index_mut(&mut self, variable: PropositionalVariable) -> &mut T {
        self.index_mut(variable.index() as usize)
    }
}

pub struct PropositionalVariableGeneratorIterator {
    current_index: u32,
    end_index: u32,
}

impl PropositionalVariableGeneratorIterator {
    pub fn new(start_index: u32, end_index: u32) -> PropositionalVariableGeneratorIterator {
        PropositionalVariableGeneratorIterator {
            current_index: start_index,
            end_index,
        }
    }
}

impl Iterator for PropositionalVariableGeneratorIterator {
    type Item = PropositionalVariable;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.end_index {
            return None;
        }

        let variable = PropositionalVariable::new(self.current_index);
        self.current_index += 1;

        Some(variable)
    }
}
