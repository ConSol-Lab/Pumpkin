use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct StateChange {
    pub(crate) old_value: i64,
    pub(crate) reference: Rc<RefCell<i64>>,
}
impl StateChange {
    pub(crate) fn undo(self) {
        *(*self.reference).borrow_mut() = self.old_value;
    }
}
