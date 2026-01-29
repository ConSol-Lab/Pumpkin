import logging
from pathlib import Path
from typing import Tuple

import click
import matplotlib.pyplot as plt
import polars as pl

from constraint_solving import common_init
from constraint_solving.config import Config
from constraint_solving.experiments import Experiment, ExperimentDoesNotExist, load_experiment


def load_csv(config: Config, experiment_name: str) -> Tuple[Experiment, Path]:
    try:
        experiment = load_experiment(config, experiment_name)
    except ExperimentDoesNotExist:
        logging.error(f"Experiment '{experiment_name}' does not exist.")
        raise Exception()

    results_dir = config.results_dir / experiment.name

    if not results_dir.is_dir():
        logging.error(f"Experiment `{experiment_name}` does not have a results dir")
        raise Exception()

    results_csv = results_dir / f"run_data_{experiment.name}.csv"
    if not results_csv.is_file():
        logging.error(f"Experiment {experiment_name} does not have a run_data file")
        raise Exception()

    return (experiment, results_csv)


def generate_image(
    config: Config, experiment_name: str, other_experiment_name: str, combined: pl.DataFrame, column_name: str
):
    image_dir = config.figures_dir / f"{experiment_name}__{other_experiment_name}"
    image_dir.mkdir(parents=True, exist_ok=True)

    plt.scatter(combined[column_name], combined[f"{column_name}_right"])
    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel(f"{column_name} {experiment_name}")
    plt.ylabel(f"{column_name} {other_experiment_name}")
    plt.title(f"{column_name} comparison")
    plt.savefig(image_dir / f"{column_name}.png")

    plt.clf()


@click.command()
@click.argument("experiment_name", type=str)
@click.argument("other_experiment_name", type=str)
def run(experiment_name: str, other_experiment_name: str) -> int:
    config = common_init()

    # Load first experiment
    try:
        (_, experiment_csv) = load_csv(config, experiment_name)
    except Exception:
        return 1

    # Load other experiment
    try:
        (_, other_experiment_csv) = load_csv(config, other_experiment_name)
    except Exception:
        return 1

    logging.info("Processing...")

    experiment_df = pl.read_csv(experiment_csv)
    other_experiment_df = pl.read_csv(other_experiment_csv)

    combined = experiment_df.join(other_experiment_df, on="benchmark-key").filter(pl.col("status") == "OPTIMAL")

    generate_image(config, experiment_name, other_experiment_name, combined, "failures")
    generate_image(config, experiment_name, other_experiment_name, combined, "solveTime")

    return 0
