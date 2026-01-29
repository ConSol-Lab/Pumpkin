import asyncio
from graphlib import TopologicalSorter
import logging
import math
from typing import List
import shlex
import time
import os
import stat

import click
import psutil
import tomli_w

from constraint_solving import Config, common_init
from constraint_solving.manifests import RunKey
from constraint_solving.experiments import (
    Experiment,
    ExperimentDoesNotExist,
    RunStatus,
    load_experiment,
    create_runs_db_if_not_exists,
)


# The number of seconds the process receives to gracefully stop after receiving a SIGINT
TERMINATION_TIMEOUT = 10


async def execute_single_run_inner(
    experiment: Experiment, run_key: RunKey, index: int, num_runs: int
):
    run = next(run for run in experiment.manifest.runs if run.key == run_key)
    cwd = experiment.path / str(run.key.benchmark_key)

    stdout_path = cwd / f"stdout.{run.key.step_name}.log"
    stderr_path = cwd / f"stderr.{run.key.step_name}.log"
    driver_path = cwd / f"driver.{run.key.step_name}.log"

    cpu_user_seconds = 0
    cpu_system_seconds = 0

    with stdout_path.open("wb") as stdout:
        with stderr_path.open("wb") as stderr:
            experiment.set_run_status(run_key, RunStatus.RUNNING)
            logging.info(f"Starting {run_key}... ({index}/{num_runs})")

            # Launch process (non-blocking startup)
            process = psutil.Popen(
                run.command,
                cwd=cwd,
                stdout=stdout,
                stderr=stderr,
            )

            start = time.monotonic()

    # Repeatedly check for completion or timeout
    while True:
        # Check if process finished
        if process.poll() is not None:
            # Last CPU times snapshot before exit
            try:
                cpu_times = process.cpu_times()
                cpu_user_seconds = cpu_times.user
                cpu_system_seconds = cpu_times.system
            except psutil.NoSuchProcess:
                pass
            break

        # Take CPU times snapshot while running
        try:
            cpu_times = process.cpu_times()
            cpu_user_seconds = cpu_times.user
            cpu_system_seconds = cpu_times.system
        except psutil.NoSuchProcess:
            # Rare race: process vanished between poll and cpu_times
            break

        # Check timeout
        elapsed = time.monotonic() - start
        if run.timeout is not None and elapsed > run.timeout:
            logging.info(f"Timeout elapsed for {run_key} ({index}/{num_runs})")
            process.terminate()
            try:
                process.wait(timeout=TERMINATION_TIMEOUT)  # give it a chance to exit
            except psutil.TimeoutExpired:
                logging.info(f"Killing {run_key} ({index}/{num_runs})")
                process.kill()
            break

        # Yield control to event loop (non-blocking sleep)
        await asyncio.sleep(1)

    # Safely wait for the final exit code inside a thread
    returncode = await asyncio.to_thread(process.wait)
    wall_clock = time.monotonic() - start

    driver_stats = {
        "command": shlex.join(run.command),
        "command-status": returncode,
        "cpu-time-user": cpu_user_seconds,
        "cpu-time-system": cpu_system_seconds,
        "cpu-time": cpu_system_seconds + cpu_user_seconds,
        "wall-clock-time": wall_clock,
    }

    logging.info(
        f"Finished {run_key} after {wall_clock:.1f} seconds ({index}/{num_runs})"
    )
    experiment.set_run_status(run_key, RunStatus.FINISHED)

    with driver_path.open("wb") as driver_log:
        tomli_w.dump(driver_stats, driver_log)


async def execute_single_run(
    config: Config,
    experiment: Experiment,
    run_key: RunKey,
    semaphore: asyncio.Semaphore,
    rerun: bool,
    index: int,
    num_runs: int,
):
    status = experiment.get_run_status(run_key)
    if not rerun and status == RunStatus.FINISHED:
        logging.info(f"Skipping {run_key} because it is {status} ({index}/{num_runs}).")
        return
    async with semaphore:
        await execute_single_run_inner(experiment, run_key, index, num_runs)


async def execute_runs(
    config: Config,
    experiment: Experiment,
    order: List[RunKey],
    rerun: bool,
    num_threads: int,
):
    sem = asyncio.Semaphore(num_threads)

    tasks = []
    for idx, run_key in enumerate(order):
        tasks.append(
            asyncio.create_task(
                execute_single_run(
                    config, experiment, run_key, sem, rerun, idx + 1, len(order)
                )
            )
        )

    await asyncio.wait(tasks)


def run_locally(
    config: Config,
    experiment: Experiment,
    rerun: bool,
    num_threads: int,
    step_name: str | None,
):
    if step_name is not None:
        order = []
        for run in experiment.manifest.runs:
            if run.key.step_name != step_name:
                continue

            order.append(run.key)
    else:
        run_queue = TopologicalSorter()

        for run in experiment.manifest.runs:
            run_queue.add(run.key, *run.depends_on)

        order = list(run_queue.static_order())

    asyncio.run(execute_runs(config, experiment, order, rerun, num_threads))


# Memory in MB
MEMORY = 4000

# The maximum size of a job array.
MAX_ARRAY_SIZE = 1000

SBATCH_FILE_TEMPLATE = """
#!/bin/bash
#SBATCH --job-name=@JOB_NAME@
#SBATCH --array=1-@NUM_JOBS@
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

#SBATCH --time=@TIME@
#SBATCH --mem-per-cpu=@MEMORY@M

#SBATCH --account=research-eemcs-st
#SBATCH --partition=compute-p2

# How far to offset the SLURM_ARRAY_TASK_ID into the commands file.
#
# Required because multiple arrays may be necessary to execute all the commands.
ID_OFFSET=@ID_OFFSET@

COMMAND_ID=$(($ID_OFFSET+$SLURM_ARRAY_TASK_ID))

# All the benchmark keys in this step.
BENCHMARK_KEYS=(@BENCHMARK_KEYS@)
BENCHMARK_KEY=${BENCHMARK_KEYS[$COMMAND_ID]}

# Read the command from the commands file. This file lists all the commands on 
# separate lines, and we use $COMMAND_ID to find the line and extract
# the command.
COMMAND=$(sed "${COMMAND_ID}q;d" @COMMANDS_FILE@)

RUN_DIR=@RUN_DIR_PREFIX@/$BENCHMARK_KEY

cd $RUN_DIR

DRIVER_LOG="./driver.@STEP_NAME@.log"
OUTPUT_LOG="./stdout.@STEP_NAME@.log"
ERROR_LOG="./stderr.@STEP_NAME@.log"

# Put the command into the driver log.
echo "command = \\\"${COMMAND}\\\"" > $DRIVER_LOG

srun /usr/bin/time \\
        --quiet \\
        --format="command-status = %x\\nwall-clock-time = %e\\ncpu-time-user = %U\\ncpu-time-system = %S" \\
        --append \\
        --output=$DRIVER_LOG \\
        $COMMAND > $OUTPUT_LOG 2> $ERROR_LOG
""".lstrip()


def setup_step_in_slurm(
    config: Config, experiment: Experiment, step_name: str, timeout: int
):
    slurm_file_dir = config.experiments_dir / "slurm" / experiment.name
    slurm_file_dir.mkdir(parents=True, exist_ok=True)

    # Generate a file with each of the commands on a new line.
    commands_file = slurm_file_dir / f"{step_name}.commands.txt"
    num_commands = 0

    benchmark_keys = set()

    with commands_file.open("w") as commands:
        for run in experiment.manifest.runs:
            if run.key.step_name != step_name:
                continue

            benchmark_keys.add(run.key.benchmark_key)
            num_commands += 1

            command_str = shlex.join(run.command)
            commands.write(f"{command_str}\n")

    assert len(benchmark_keys) == num_commands, (
        "Every benchmark key must be part of every run"
    )

    # Create the submission files for slurm.
    num_job_arrays = math.ceil(num_commands / MAX_ARRAY_SIZE)

    for i in range(num_job_arrays):
        slurm_file = slurm_file_dir / f"{step_name}.array_{i + 1}.job"

        id_offset = i * MAX_ARRAY_SIZE
        num_jobs = min(MAX_ARRAY_SIZE, num_commands - id_offset)

        with slurm_file.open("w") as slurm:
            seconds = timeout % (24 * 3600)
            hour = seconds // 3600
            seconds %= 3600
            minutes = seconds // 60
            seconds %= 60

            benchmark_keys_str = " ".join(f'"{key}"' for key in benchmark_keys)

            contents = (
                SBATCH_FILE_TEMPLATE.replace("@ID_OFFSET@", str(id_offset))
                .replace("@STEP_NAME@", step_name)
                .replace("@RUN_DIR_PREFIX@", str(experiment.path))
                .replace("@BENCHMARK_KEYS@", benchmark_keys_str)
                .replace("@JOB_NAME@", f"hl-{step_name}")
                .replace("@COMMANDS_FILE@", str(commands_file))
                .replace("@TIME@", f"{hour}:{minutes}:{seconds}")
                .replace("@MEMORY@", str(MEMORY))
                .replace("@NUM_JOBS@", str(num_jobs))
            )

            slurm.write(contents)

    submit_file = slurm_file_dir / f"submit_{step_name}.sh"
    with submit_file.open("w") as submit:
        submit.write("#!/bin/bash\n\n")

        for i in range(num_job_arrays):
            submit.write(f"sbatch {slurm_file_dir}/{step_name}.array_{i + 1}.job\n")

    st = os.stat(submit_file)
    os.chmod(submit_file, st.st_mode | stat.S_IEXEC)

    pass


def run_slurm(config: Config, experiment: Experiment):
    # TODO: Make this proper, this is very buggy.

    # Slurm parameter. If a run does not have a timeout, then we say it is 30 minutes.
    FALLBACK_TIMEOUT = 30 * 60

    # Map from steps to timeout. We assume all runs in the same step have the same timeout.
    steps = {run.key.step_name: run.timeout for run in experiment.manifest.runs}

    for step, timeout in steps.items():
        setup_step_in_slurm(config, experiment, step, timeout or FALLBACK_TIMEOUT)


@click.command()
@click.option("--rerun", is_flag=True)
@click.option("--num-threads", type=int, default=1)
@click.option("--step-name", type=str)
@click.option("--slurm", is_flag=True)
@click.argument("experiment_name", type=str)
def run(
    experiment_name: str,
    rerun: bool,
    num_threads: int,
    step_name: str | None,
    slurm: bool,
) -> int:
    config = common_init()

    try:
        experiment = load_experiment(config, experiment_name)
    except ExperimentDoesNotExist:
        logging.error(f"Experiment '{experiment_name}' does not exist.")
        return 1

    logging.info("Creating runs database...")
    create_runs_db_if_not_exists(config, experiment)
    logging.info("Successfully created runs database.")

    if slurm:
        logging.info("Generating slurm files...")
        run_slurm(config, experiment)
        logging.info("Done. You can submit the slurm jobs.")
    else:
        if step_name is None:
            logging.info("Starting local execution...")
        else:
            logging.info(f"Starting local execution of step {step_name}...")
        run_locally(config, experiment, rerun, num_threads, step_name)
        logging.info("Finished all runs.")

    return 0
