use crate::engine::variables::PropositionalVariable;

#[derive(Debug, Copy, Clone)]
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
        if self.current_index >= self.end_index {
            return None;
        }

        let variable = PropositionalVariable::new(self.current_index);
        self.current_index += 1;

        Some(variable)
    }
}
