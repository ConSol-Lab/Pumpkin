use pumpkin_lib::variables::AffineView;
use pumpkin_lib::variables::DomainId;
use pumpkin_lib::variables::TransformableVariable;
use pyo3::pyclass;
use pyo3::pymethods;

/// An integer variable.
#[pyclass]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Variable(pub(crate) AffineView<DomainId>);

#[pymethods]
impl Variable {
    fn offset(&self, offset: i32) -> Variable {
        Variable(self.0.offset(offset))
    }
}
