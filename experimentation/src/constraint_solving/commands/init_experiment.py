import logging
import os
import shlex
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Dict, List

import click
import git
import inquirer
from rich import console, progress

from constraint_solving import Config, common_init
from constraint_solving.benchmarks import Benchmark, BenchmarkSetDoesNotExist, load_benchmark_set
from constraint_solving.manifests import Manifest, Run, RunKey, VersionedSource, save_manifest
from constraint_solving.pipelines import PipelineDoesNotExist, load_pipeline
from constraint_solving.sources import BinaryNotInAnySource, Source, resolve_sources


def create_command(template: str, benchmark: Benchmark, binary_paths: Dict[str, Path]) -> List[str]:
    parts = shlex.split(template)

    final_command = []
    for part in parts:
        if part.startswith("@bench:"):
            # Paths are relative to the run directory, which is inside:
            # experiments/outputs/<experiment_name>/<step_name>
            index = int(part.removeprefix("@bench:"))
            final_command.append(f"../../../../benchmarks/{benchmark.arguments[index - 1]}")
        elif part.startswith("@bin:"):
            binary_name = part.removeprefix("@bin:")

            final_command.append(f"../../../../{binary_paths[binary_name]}")
        else:
            final_command.append(part)

    return final_command


class SourceCompileError(Exception):
    source: Source

    def __init__(self, source: Source, *args: object) -> None:
        super().__init__(*args)
        self.source = source


class GitRemoteProgress(git.RemoteProgress):
    """Courtesy of https://stackoverflow.com/a/71285627/8369985."""

    OP_CODES = [
        "BEGIN",
        "CHECKING_OUT",
        "COMPRESSING",
        "COUNTING",
        "END",
        "FINDING_SOURCES",
        "RECEIVING",
        "RESOLVING",
        "WRITING",
    ]
    OP_CODE_MAP = {getattr(git.RemoteProgress, _op_code): _op_code for _op_code in OP_CODES}

    def __init__(self) -> None:
        super().__init__()
        self.progressbar = progress.Progress(
            progress.SpinnerColumn(),
            # *progress.Progress.get_default_columns(),
            progress.TextColumn("[progress.description]{task.description}"),
            progress.BarColumn(),
            progress.TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
            "eta",
            progress.TimeRemainingColumn(),
            progress.TextColumn("{task.fields[message]}"),
            console=console.Console(),
            transient=False,
        )
        self.progressbar.start()
        self.active_task = None

    def __del__(self) -> None:
        # logger.info("Destroying bar...")
        self.progressbar.stop()

    @classmethod
    def get_curr_op(cls, op_code: int) -> str:
        """Get OP name from OP code."""
        # Remove BEGIN- and END-flag and get op name
        op_code_masked = op_code & cls.OP_MASK
        return cls.OP_CODE_MAP.get(op_code_masked, "?").title()

    def update(
        self,
        op_code: int,
        cur_count: str | float,
        max_count: str | float | None = None,
        message: str | None = "",
    ) -> None:
        # Start new bar on each BEGIN-flag
        if op_code & self.BEGIN:
            self.curr_op = self.get_curr_op(op_code)
            # logger.info("Next: %s", self.curr_op)
            self.active_task = self.progressbar.add_task(
                description=self.curr_op,
                total=max_count,
                message=message,
            )

        self.progressbar.update(
            task_id=self.active_task,
            completed=cur_count,
            message=message,
        )

        # End progress monitoring on each END-flag
        if op_code & self.END:
            # logger.info("Done: %s", self.curr_op)
            self.progressbar.update(
                task_id=self.active_task,
                message=f"[bright_black]{message}",
            )


def initialise_binaries(config: Config, sources: List[Source]) -> List[VersionedSource]:
    versions = inquirer.prompt(
        [inquirer.Text(source.name, f"Provide the ref for '{source.name}'") for source in sources]
    )

    assert versions is not None

    versioned_sources = []

    for source in sources:
        version: str = versions[source.name]
        versioned_source_path = config.experiments_solvers_dir / source.name / version

        if versioned_source_path.is_dir():
            if inquirer.confirm("Version already exists. Recompile?"):
                shutil.rmtree(versioned_source_path)
            else:
                versioned_sources.append(VersionedSource(source.name, version, path=versioned_source_path))
                continue

        versioned_source_path.mkdir(parents=True)

        source_path = Path(source.git_url).absolute().resolve()
        repo = git.Repo.clone_from(
            source_path,
            versioned_source_path,
            progress=GitRemoteProgress(),
        )
        repo.git.checkout(version)

        return_code = subprocess.call(
            shlex.split(source.compile_command),
            cwd=versioned_source_path,
            stdout=sys.stdout,
            stderr=sys.stderr,
        )

        if return_code != 0:
            raise SourceCompileError(source)

        versioned_sources.append(VersionedSource(source.name, version, path=versioned_source_path))

    return versioned_sources


def resolve_binary_paths(
    binaries: List[str],
    sources: List[Source],
    versioned_sources: List[VersionedSource],
) -> Dict[str, Path]:
    paths = {}

    for binary_name in binaries:
        (source_for_binary, binary) = next(
            (source, binary) for source in sources for binary in source.binaries if binary.name == binary_name
        )
        versioned_source = next(source for source in versioned_sources if source.name == source_for_binary.name)

        paths[binary_name] = versioned_source.path / binary.path

    return paths


@click.command()
@click.argument("pipeline_name", type=str)
@click.argument("benchmark_set_name", type=str)
def run(pipeline_name: str, benchmark_set_name: str) -> int:
    config = common_init()

    try:
        pipeline = load_pipeline(config, pipeline_name)
    except PipelineDoesNotExist:
        logging.error(f"Pipeline '{pipeline_name}' does not exist.")
        return 1

    try:
        benchmark_set = load_benchmark_set(config, benchmark_set_name)
    except BenchmarkSetDoesNotExist:
        logging.error(f"Benchmark set '{benchmark_set_name}' does not exist.")
        return 1

    try:
        sources = resolve_sources(config, pipeline.binaries)
    except BinaryNotInAnySource as e:
        logging.error(f"Binary '{e.binary_name}' is not provided by any source.")
        return 1

    logging.info(f"Initializing pipeline {pipeline.name}")

    try:
        versioned_sources = initialise_binaries(config, sources)
    except SourceCompileError as e:
        logging.error(f"Failed to compile source {e.source.name}")
        return 1

    binary_paths = resolve_binary_paths(pipeline.binaries, sources, versioned_sources)

    experiment_name = inquirer.prompt(
        [
            inquirer.Text("name", message="What is the name of this experiment?"),
        ]
    )["name"]

    experiment_output: Path = config.experiments_output_dir / experiment_name

    try:
        experiment_output.mkdir(parents=True)
    except FileExistsError:
        logging.error(f"Experiment with name '{experiment_name}' already exists.")
        return 1

    manifest = Manifest(experiment_name, sources=versioned_sources)

    for benchmark in benchmark_set:
        benchmark_output_path = experiment_output / benchmark.key

        try:
            benchmark_output_path.mkdir(parents=True)
        except FileExistsError:
            logging.error(f"Benchmark '{benchmark_set_name}' contains duplicate keys.")
            return 1

        for step in pipeline.steps:
            manifest.runs.append(
                Run(
                    key=RunKey(step.name, benchmark.key),
                    command=create_command(step.command, benchmark, binary_paths),
                    timeout=step.timeout,
                    depends_on=list(
                        map(
                            lambda step_name: RunKey(step_name, benchmark.key),
                            step.depends_on,
                        )
                    ),
                )
            )

    save_manifest(config, manifest)

    logging.info(f"Initialized {len(manifest.runs)} runs.")

    return 0
