use std::fmt::Debug;
use std::fmt::Formatter;

#[derive(Copy, Clone)]
pub(crate) struct DebugDyn<'a> {
    trait_name: &'a str,
}

impl<'a> DebugDyn<'a> {
    pub(crate) fn from(trait_name: &'a str) -> Self {
        DebugDyn { trait_name }
    }
}

impl Debug for DebugDyn<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<dyn {}>", self.trait_name)
    }
}
