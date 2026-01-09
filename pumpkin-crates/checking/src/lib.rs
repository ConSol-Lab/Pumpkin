use std::fmt::Debug;

use dyn_clone::DynClone;
use dyn_clone::clone_trait_object;

pub trait InferenceChecker: Debug + DynClone {
    fn check(&self) -> bool;
}

// Allow Box<dyn InferenceChecker> to be cloned
clone_trait_object!(InferenceChecker);
