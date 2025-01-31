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
