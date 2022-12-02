#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct IntegerVariable {
    pub id: u32,
}

impl<T> std::ops::Index<IntegerVariable> for Vec<T> {
    type Output = T;
    fn index(&self, index_variable: IntegerVariable) -> &T {
        self.index(index_variable.id as usize)
    }
}

impl<T> std::ops::IndexMut<IntegerVariable> for Vec<T> {
    fn index_mut(&mut self, index_variable: IntegerVariable) -> &mut T {
        self.index_mut(index_variable.id as usize)
    }
}

impl std::fmt::Display for IntegerVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.id)
    }
}

pub struct IntegerVariableGeneratorIterator {
    current_index: u32,
    end_index: u32,
}

impl IntegerVariableGeneratorIterator {
    pub fn new(start_index: u32, end_index: u32) -> IntegerVariableGeneratorIterator {
        IntegerVariableGeneratorIterator {
            current_index: start_index,
            end_index,
        }
    }
}

impl Iterator for IntegerVariableGeneratorIterator {
    type Item = IntegerVariable;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.end_index {
            return None;
        }

        let variable = IntegerVariable {
            id: self.current_index,
        };
        self.current_index += 1;

        Some(variable)
    }
}
