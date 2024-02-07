use super::StorageKey;

#[derive(Clone, PartialEq, Eq, Copy, Hash)]
pub struct DomainId {
    pub id: u32,
}

impl DomainId {
    pub fn new(id: u32) -> Self {
        DomainId { id }
    }
}

impl StorageKey for DomainId {
    fn index(&self) -> usize {
        self.id as usize
    }
}

impl std::fmt::Display for DomainId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.id)
    }
}

impl std::fmt::Debug for DomainId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "x{}", self.id)
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
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
    type Item = DomainId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.end_index {
            return None;
        }

        let variable = DomainId {
            id: self.current_index,
        };
        self.current_index += 1;

        Some(variable)
    }
}
