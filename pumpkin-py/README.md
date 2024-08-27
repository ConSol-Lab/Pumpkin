# Pumpkin-py

The python interface for the Pumpkin solver.

## Contributing

When contributing to the python interface, two tools should be installed in
the current python environment:
1. `maturin`, which builds and installs the python module
2. `pytest`, to run the tests.

To build the `pumpkin_py` module and make it available to the current python
environment, run:
```
maturin develop
```
This command will compile the Rust binary. This means that whenever the Rust 
code changes, maturin has to re-compile before those changes are visible to
the python module. This is not the case for the python code. Changes to that
are immediately visible in the environment, without needing to run `maturin`
again.

Then, the examples (for example `nqueens`) can be run with 
```
python python/examples/nqueens.py 5
```
