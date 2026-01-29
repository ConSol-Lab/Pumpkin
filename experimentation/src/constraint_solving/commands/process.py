import csv
import logging
import shutil
import tomllib
from pathlib import Path

import click
import tomli_w
from rich import progress

from constraint_solving import common_init
from constraint_solving.experiments import ExperimentDoesNotExist, load_experiment
from constraint_solving.manifests import save_manifest

OPTIMAL_MARKER = 10 * "="
SOLUTION_MARKER = 10 * "-"


REQUIRED_STATS = [
    "command",
    "benchmark-key",
    "command-status",
    "status",
]


def find_second_last(text: str, pattern: str) -> int:
    return text.rfind(pattern, 0, text.rfind(pattern))


def parse_solver_log(directory: Path, step_name: str) -> dict | None:
    stdout_path = directory / f"stdout.{step_name}.log"
    stderr_path = directory / f"stderr.{step_name}.log"
    driver_path = directory / f"driver.{step_name}.log"

    print(driver_path)
    if not driver_path.is_file():
        driver_stats = {}
    else:
        with driver_path.open("rb") as fp:
            driver_stats = tomllib.load(fp)

    status = "UNKNOWN"

    if stdout_path.is_file():
        with stdout_path.open("r") as fp:
            stdout = fp.read()
    else:
        stdout = ""

    if stderr_path.is_file():
        with stderr_path.open("r") as fp:
            stderr = fp.read()
    else:
        stderr = ""

    if OPTIMAL_MARKER in stdout:
        status = "OPTIMAL"
    elif "UNSATISFIABLE" in stdout:
        status = "UNSATISFIABLE"
    elif SOLUTION_MARKER in stdout:
        status = "SATISFIABLE"
    elif "ERROR" in stdout or len(stderr) > 0:
        status = "ERROR"

    last_statistics_block_start = max(find_second_last(stdout, "%%%mzn-stat-end"), 0)
    last_statistics_block = stdout[last_statistics_block_start:]

    statistics = {}

    for line in last_statistics_block.splitlines():
        if line.startswith("%%%mzn-stat: "):
            stat = line.removeprefix("%%%mzn-stat: ")
            key, value = stat.split("=")
            statistics[key] = float(value)

    return {
        **driver_stats,
        "status": status,
        **statistics,
        "encountered-overflow": "integer overflow" in stderr,
    }


@click.command()
@click.argument("experiment_name", type=str)
def run(experiment_name: str) -> int:
    config = common_init()

    try:
        experiment = load_experiment(config, experiment_name)
    except ExperimentDoesNotExist:
        logging.error(f"Experiment '{experiment_name}' does not exist.")
        return 1

    finished_runs = experiment.finished_runs()
    logging.error(f"Processing {len(finished_runs)} out of {len(experiment.manifest.runs)} runs.")

    results_dir = config.results_dir / experiment.name
    results_dir.mkdir(parents=True, exist_ok=True)

    save_manifest(config, experiment.manifest, file_path=results_dir / "manifest.toml")

    logging.info("Processing...")

    aggregated_stats = []

    for benchmark_run_dir in progress.track(
        experiment.path.glob("*/"),
        description="Parsing runs...",
    ):
        log_stats = parse_solver_log(benchmark_run_dir, "solve")
        assert log_stats is not None

        stats = {"benchmark-key": benchmark_run_dir.name, **log_stats}

        stats_file_path = benchmark_run_dir / "stats.toml"
        with stats_file_path.open("wb") as fp:
            tomli_w.dump(stats, fp)

        aggregated_stats.append(stats)

    logging.info("Writing aggregated CSV...")
    features = set()
    for row in aggregated_stats:
        features.update(row.keys())
    for stat in REQUIRED_STATS:
        if stat in features:
            features.remove(stat)
    fields = REQUIRED_STATS + sorted(features)

    csv_file_path = results_dir / f"run_data_{experiment.name}.csv"
    with csv_file_path.open("w") as fp:
        writer = csv.DictWriter(fp, fieldnames=fields)
        writer.writeheader()
        writer.writerows(aggregated_stats)

    logging.info("Archiving run data...")
    shutil.make_archive(str(results_dir / "data"), "zip", root_dir=experiment.path)

    return 0
