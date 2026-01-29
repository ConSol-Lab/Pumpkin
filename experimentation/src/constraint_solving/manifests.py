from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Self, Tuple
import tomllib

import tomli_w


from constraint_solving.config import Config


@dataclass
class RunKey:
    step_name: str
    benchmark_key: str

    @classmethod
    def from_str(cls, serialized: str) -> Self:
        step_name, benchmark_key = serialized.split(".")
        return cls(step_name=step_name, benchmark_key=benchmark_key)

    def __str__(self) -> str:
        return f"{self.step_name}.{self.benchmark_key}"

    def __eq__(self, other) -> bool:
        return (
            isinstance(other, RunKey)
            and self.step_name == other.step_name
            and self.benchmark_key == other.benchmark_key
        )

    def __hash__(self) -> int:
        return hash((self.step_name, self.benchmark_key))


@dataclass
class Run:
    key: RunKey
    command: List[str]
    timeout: int | None
    depends_on: List[RunKey]


@dataclass
class VersionedSource:
    name: str
    version: str
    path: Path


@dataclass
class Manifest:
    name: str
    sources: List[VersionedSource] = field(default_factory=list)
    runs: List[Run] = field(default_factory=list)


def serialize_run_key(run_key: RunKey) -> str:
    return str(run_key)


def serialize_versioned_source(source: VersionedSource) -> Dict[str, str]:
    return {
        "path": str(source.path),
        "name": source.name,
        "version": source.version,
    }


def make_versioned_source(source_toml) -> VersionedSource:
    return VersionedSource(
        name=source_toml["name"],
        version=source_toml["version"],
        path=Path(source_toml["path"]),
    )


def serialize_run(run: Run) -> Tuple[str, dict]:
    run_key = serialize_run_key(run.key)

    timeout_item = {"timeout": run.timeout} if run.timeout is not None else {}

    return run_key, {
        "command": run.command,
        "depends-on": list(map(serialize_run_key, run.depends_on)),
        **timeout_item,
    }


def make_run(run_item) -> Run:
    run_key = RunKey.from_str(run_item[0])

    contents = run_item[1]
    depends_on = list(map(RunKey.from_str, contents["depends-on"]))

    return Run(
        run_key,
        command=contents["command"],
        depends_on=depends_on,
        timeout=contents["timeout"] if "timeout" in contents else None,
    )


def load_manifest(config: Config, experiment_name: str) -> Manifest:
    manifest_path = config.manifests_dir / f"{experiment_name}.toml"

    with manifest_path.open("rb") as fp:
        toml_contents = tomllib.load(fp)

    sources = list(map(make_versioned_source, toml_contents["sources"]))
    runs = list(map(make_run, toml_contents["runs"].items()))

    return Manifest(experiment_name, sources, runs)


def save_manifest(config: Config, manifest: Manifest, file_path: Path | None = None):
    manifest_path = (
        file_path
        if file_path is not None
        else config.manifests_dir / f"{manifest.name}.toml"
    )

    serialized_manifest = {
        "sources": list(map(serialize_versioned_source, manifest.sources)),
        "runs": dict(map(serialize_run, manifest.runs)),
    }

    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    with manifest_path.open("wb") as fp:
        tomli_w.dump(serialized_manifest, fp)
