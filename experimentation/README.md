For running, we require an installation of [uv](https://docs.astral.sh/uv/).

# Assignment 1

# Initialising Experiments
The running and processing of the experiments is the same, but the initialisation is different for each experiment. We will first describe the initialisation of the experiments.

## Cumulative Propagator
Please run the following commands:
```bash
uv run init_experiment solve_cumulative rcpsp
```

## Cumulative Conflict Only
Please run the following commands:
```bash
uv run init_experiment solve_cumulative_conflict rcpsp
```

## Linear Propagator
Please run the following commands:
```bash
uv run init_experiment solve_linear linear
```

## Cumulative Conflict Only
Please run the following commands:
```bash
uv run init_experiment solve_linear_conflict linear
```

# Run experiments
Now that you have initialised the experiments (with the name `<EXPERIMENT_NAME>`), you can run the experiments using the following command:
```bash
uv run execute_experiment <EXPERIMENT_NAME>
```

If you want to run multiple instances at the same time, then you can use the following command:
```bash
uv run execute_experiment --num-threads=<NUM_THREADS> <EXPERIMENT_NAME>
```

# Process Results
You can generate statistics for the experiments using the following command:
```bash
uv run process <EXPERIMENT_NAME>
```
This will create a folder in `results/<EXPERIMENT_NAME>` containing parsed results.

# Create figures
You can create the figures to include in the report by running the following command:
```bash
uv run generate_figures <EXPERIMENT_NAME> <OTHER_EXPERIMENT_NAME>
```
This will create a scatter plot between the optimally solved instances for both experiments for the following statistics:
1) number of failures
2) solve time 
These plots are created in the folder `figures/<EXPERIMENT_NAME>__<OTHER_EXPERIMENT_NAME>/` .
