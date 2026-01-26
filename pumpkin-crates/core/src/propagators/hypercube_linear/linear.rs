use std::num::NonZero;

use crate::variables::{AffineView, DomainId};

/// The linear inequality part of a hypercube linear constraint.
#[derive(Clone, Debug)]
pub struct LinearInequality {
    terms: Box<[AffineView<DomainId>]>,
    bound: i32,
}

impl LinearInequality {
    pub fn new(terms: impl IntoIterator<Item = (NonZero<i32>, DomainId)>, bound: i32) -> Self {
        todo!()
    }
}
