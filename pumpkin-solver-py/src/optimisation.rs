use pyo3::prelude::*;

use crate::result::Solution;

#[pyclass(from_py_object)]
#[derive(Clone)]
pub enum OptimisationResult {
    /// The problem was solved to optimality, and the solution is an optimal one.
    Optimal(Solution),
    /// At least one solution was identified, and the solution is the best one.
    Satisfiable(Solution),
    /// The problem was unsatisfiable.
    Unsatisfiable(),
    /// None of the other variants were concluded.
    Unknown(),
}

#[pyclass(eq, eq_int, from_py_object)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Optimiser {
    LinearSatUnsat,
    LinearUnsatSat,
}

#[pyclass(eq, eq_int, from_py_object)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Minimise,
    Maximise,
}

pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Optimiser>()?;
    m.add_class::<Direction>()?;
    m.add_class::<OptimisationResult>()?;
    Ok(())
}
