use std::fmt::Debug;

use crate::engine::propagation::LocalId;

#[derive(Debug, Clone)]
pub struct ArgDisjunctiveTask<Var> {
    pub start_time: Var,
    pub processing_time: i32,
}

#[derive(Clone, Debug)]
pub(super) struct DisjunctiveTask<Var> {
    pub(crate) start_time: Var,
    pub(crate) processing_time: i32,
    pub(crate) id: LocalId,
}
