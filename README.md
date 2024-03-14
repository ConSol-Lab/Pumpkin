# Pumpkin Solver

A general-purpose combinatorial optimisation solver, using (Max)SAT and CP 
techniques.

## Cloning the project

To clone the project, run:
```sh 
$ git clone git@bitbucket.org:EmirD/pumpkin-private.git
```

To ensure you do not commit code that does not pass `cargo +nightly fmt` or `cargo 
clippy`, there is a pre-commit hook that runs these commands.
If you are running Linux/MacOS/WSL/Git Bash, copy the hooks in `.githooks` to 
the `.git` folder:
```sh 
$ find .githooks -type f -exec ln -sf ../../{} .git/hooks/ \;
```

## Toolchain requirements

The project is built with a stable release of Rust (1.72.1+). _However,_ we use the nightly
toolchain for code formatting, to keep imports line-based for easier merging with Git. You can
install the nightly toolchain including rustfmt with `rustup toolchain install --component rustfmt
-- nightly`, then use `cargo +nightly fmt` to select the nightly toolchain just for formatting.
This is also how the pre-commit hooks and the CI work.
