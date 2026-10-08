mod conflict;

use std::cell::RefCell;

use crate::Union;

#[derive(Clone, Debug)]
pub struct ElementChecker<VX, VI, VE> {
    array: Box<[VX]>,
    index: VI,
    rhs: VE,

    union: RefCell<Union>,
}

impl<VX, VI, VE> ElementChecker<VX, VI, VE> {
    /// Create a new [`ElementChecker`].
    pub fn new(array: Box<[VX]>, index: VI, rhs: VE) -> Self {
        ElementChecker {
            array,
            index,
            rhs,
            union: RefCell::new(Union::empty()),
        }
    }
}
