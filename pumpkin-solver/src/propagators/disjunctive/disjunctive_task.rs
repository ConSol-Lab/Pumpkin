use std::fmt::Debug;

use crate::engine::propagation::LocalId;

pub(crate) struct ArgDisjunctiveTask<Var> {
    pub(crate) start_variable: Var,
    pub(crate) processing_time: i32,
}

#[derive(Clone)]
pub(super) struct DisjunctiveTask<Var> {
    pub(crate) start_variable: Var,
    pub(crate) processing_time: i32,
    pub(crate) id: LocalId,
}

impl<Var> Debug for DisjunctiveTask<Var> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DisjunctiveTask")
            .field("d", &self.processing_time)
            .field("id", &self.id)
            .finish()
    }
}
