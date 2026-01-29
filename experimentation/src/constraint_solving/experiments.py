import sqlite3
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import List

from constraint_solving import Config
from constraint_solving.manifests import Manifest, RunKey, load_manifest

RUNS_DB_FILE = "runs.db"


class RunStatus(Enum):
    READY = "READY"
    RUNNING = "RUNNING"
    FINISHED = "FINISHED"
    KILLED = "KILLED"

    def __str__(self) -> str:
        return self.value


@dataclass
class Experiment:
    path: Path
    manifest: Manifest

    @property
    def runs_db(self) -> Path:
        return self.path / RUNS_DB_FILE

    @property
    def name(self) -> str:
        return self.path.name

    def finished_runs(self) -> List[RunKey]:
        with sqlite3.connect(self.runs_db) as con:
            cursor = con.cursor()
            result = cursor.execute("SELECT key FROM runs WHERE status = 'FINISHED'")
            return [RunKey.from_str(row[0]) for row in result.fetchall()]

    def set_run_status(self, run_key: RunKey, status: RunStatus):
        with sqlite3.connect(self.runs_db) as con:
            con.execute("UPDATE runs SET status = ? WHERE key = ?", (str(status), str(run_key)))
            con.commit()

    def get_run_status(self, run_key: RunKey) -> RunStatus:
        with sqlite3.connect(self.runs_db) as con:
            cursor = con.cursor()
            result = cursor.execute("SELECT status FROM runs WHERE key = ?", (str(run_key),))

            print(run_key)

            return RunStatus(result.fetchone()[0])


class ExperimentDoesNotExist(Exception):
    pass


def create_runs_db_if_not_exists(config: Config, experiment: Experiment):
    manifest = load_manifest(config, experiment.name)
    with sqlite3.connect(experiment.runs_db) as con:
        con.execute("CREATE TABLE IF NOT EXISTS runs(key TEXT PRIMARY KEY, status TEXT);")

        rows = [(str(run.key), "READY") for run in manifest.runs]
        con.executemany("INSERT OR IGNORE INTO runs VALUES (?, ?);", rows)
        con.commit()


def load_experiment(config: Config, experiment_name: str) -> Experiment:
    experiment_path = config.experiments_output_dir / experiment_name
    if not experiment_path.is_dir():
        raise ExperimentDoesNotExist

    manifest = load_manifest(config, experiment_name)

    return Experiment(experiment_path, manifest)
