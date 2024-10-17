use crate::engine::variables::DomainId;

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct DomainGeneratorIterator {
    current_index: u32,
    end_index: u32,
}

impl DomainGeneratorIterator {
    pub fn new(start_index: u32, end_index: u32) -> DomainGeneratorIterator {
        DomainGeneratorIterator {
            current_index: start_index,
            end_index,
        }
    }
}

impl Iterator for DomainGeneratorIterator {
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
