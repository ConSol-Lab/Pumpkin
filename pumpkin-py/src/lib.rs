mod constraints;
mod model;
mod optimisation;
mod result;
mod variables;

use pyo3::prelude::*;

macro_rules! submodule {
    ($module:ident, $python:ident, $m:ident) => {{
        let submodule = PyModule::new($m.py(), stringify!($module))?;

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
    m.add_class::<variables::IntExpression>()?;
    m.add_class::<variables::BoolExpression>()?;
    m.add_class::<model::Comparator>()?;
    m.add_class::<model::Model>()?;
    m.add_class::<result::SatisfactionResult>()?;
    m.add_class::<result::Solution>()?;

    submodule!(constraints, python, m);
    submodule!(optimisation, python, m);

    Ok(())
}
