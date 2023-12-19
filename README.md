# Pumpkin Solver

A general-purpose combinatorial optimisation solver, using (Max)SAT and CP 
techniques.

## Cloning the project

To clone the project, run:
```sh 
$ git clone git@bitbucket.org:EmirD/pumpkin-private.git
```

To ensure you do not commit code that does not pass `cargo fmt` or `cargo 
clippy`, there is a pre-commit hook that runs these commands.
If you are running Linux/MacOS/WSL/Git Bash, copy the hooks in `.githooks` to 
the `.git` folder:
```sh 
$ find .githooks -type f -exec ln -sf ../../{} .git/hooks/ \;
```
