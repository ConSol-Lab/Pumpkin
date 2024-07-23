use std::marker::PhantomData;

use super::linear_less_or_equal_incremental::IncrementalLinearLessOrEqualPropagator;
use super::linear_less_or_equal_regular::LinearLessOrEqualPropagator;
use crate::variables::IntegerVariable;

#[derive(Debug)]
pub(crate) struct LinearLessOrEqualConstructor<Var, T> {
    pub(crate) x: Box<[Var]>,
    pub(crate) c: i32,

    propagator_type: PhantomData<T>,
}

impl<Var: IntegerVariable + 'static, T> LinearLessOrEqualConstructor<Var, T> {
    pub(crate) fn new(x: Box<[Var]>, c: i32) -> Self {
        LinearLessOrEqualConstructor {
            x,
            c,
            propagator_type: PhantomData,
        }
    }
}

/// An alias used for calling the [`LinearLessOrEqualConstructor::new`] method with the concrete
/// propagator type of [`LinearLessOrEqualPropagator`]; this is used to prevent creating a different
/// `new` method for each type `T`
pub(crate) type LinearLessOrEqual<Var> =
    LinearLessOrEqualConstructor<Var, LinearLessOrEqualPropagator<Var>>;

/// An alias used for calling the [`LinearLessOrEqualConstructor::new`] method with the concrete
/// propagator type of [`IncrementalLinearLessOrEqualPropagator`]; this is used to prevent creating
/// a different `new` method for each type `T`
pub(crate) type IncrementalLinearLessOrEqual<Var> =
    LinearLessOrEqualConstructor<Var, IncrementalLinearLessOrEqualPropagator<Var>>;
