mod conflict;

use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct NogoodChecker<Atomic> {
    pub nogood: Box<[Atomic]>,
}
