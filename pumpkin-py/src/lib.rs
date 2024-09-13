mod constraints;
mod core;
mod solver;

use pyo3::prelude::*;

macro_rules! submodule {
    ($module:ident, $python:ident, $m:ident) => {{
        let submodule = PyModule::new_bound($m.py(), stringify!($module))?;

        // See https://github.com/PyO3/pyo3/issues/1517#issuecomment-808664021
        pyo3::py_run!(
            $python,
            submodule,
            &format!(
                "import sys; sys.modules['pumpkin_py.{}'] = submodule",
                stringify!($module)
            )
        );

        $module::register(&submodule)?;
    }};
}

#[pymodule]
fn pumpkin_py(python: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<core::Variable>()?;
    m.add_class::<solver::Solver>()?;
    m.add_class::<solver::SatisfactionResult>()?;
    m.add_class::<solver::Solution>()?;

    submodule!(constraints, python, m);

    Ok(())
}
