use std::fmt::Debug;

#[cfg(doc)]
use crate::constraints;
use pumpkin_core::propagation::LocalId;

/// Defines the input of the [`constraints::disjunctive_strict`] constraint.
///
/// Each task has a variable starting time and a constant processing time.
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
