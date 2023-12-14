pub struct SparseSet<T> {
    size: usize,
    domain: Vec<T>,
    indices: Vec<usize>,
}

pub trait GetIndex {
    fn get_index(&self) -> usize;
}

impl<T: GetIndex> SparseSet<T> {
    pub fn new(input: Vec<T>) -> Self {
        let input_len = input.len();
        SparseSet {
            size: input_len,
            domain: input,
            indices: (0..input_len).collect::<Vec<_>>(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn has_next(&self, index: usize) -> bool {
        index < self.size
    }

    pub fn get(&self, index: usize) -> &T {
        &self.domain[index]
    }

    fn swap(&mut self, i: usize, j: usize) {
        self.domain.swap(i, j);
        self.indices[self.domain[i].get_index()] = i;
        self.indices[self.domain[j].get_index()] = j;
    }

    pub fn remove(&mut self, to_remove: &T) {
        if self.indices[to_remove.get_index()] < self.size {
            self.size -= 1;
            self.swap(self.indices[to_remove.get_index()], self.size);
        }
    }
}
