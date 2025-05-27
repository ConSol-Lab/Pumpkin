# Pumpkin Python Interface

The python interface for the Pumpkin solver.

## Contributing

When contributing to the python interface, two tools should be installed in
the current python environment:
1. `maturin`, which builds and installs the python module
2. `pytest`, to run the tests.

To build the `pumpkin_solver_py` module and make it available to the current python
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
python examples/nqueens.py 5
```

### PyO3 rebuilds

When developing in an IDE that runs `cargo check` on save, the PyO3 build 
cache can get invalidated unnecessarily. See https://github.com/PyO3/pyo3/issues/1708
for more details. One way to fix this is by making `rust-analyzer` use a
different directory. In VSCode, you could fix this by adding the following
to your `.vscode/settings.json` (in the main project directory):

```json
{
    "rust-analyzer.server.extraEnv": {
        "CARGO_TARGET_DIR": "target/analyzer"
    },
    "rust-analyzer.check.extraArgs": [
        "--target-dir=target/analyzer"
    ]
}
```
