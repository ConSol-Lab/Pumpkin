from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List

import tomllib

from constraint_solving.config import Config


@dataclass
class Step:
    name: str
    command: str
    timeout: int | None
    depends_on: List[str] = field(default_factory=list)


@dataclass
class Pipeline:
    name: str
    binaries: List[str] = field(default_factory=list)
    steps: List[Step] = field(default_factory=list)


class PipelineDoesNotExist(Exception):
    pass


def make_step(step_toml: Dict[str, Any]) -> Step:
    return Step(
        name=step_toml["name"],
        command=step_toml["command"],
        timeout=int(step_toml["timeout"]) if "timeout" in step_toml else None,
        depends_on=step_toml["depends-on"] if "depends-on" in step_toml else [],
    )


def load_pipeline(config: Config, pipeline_name: str) -> Pipeline:
    pipeline_path: Path = config.pipeline_dir / f"{pipeline_name}.toml"

    if not pipeline_path.is_file():
        raise PipelineDoesNotExist

    with pipeline_path.open("rb") as fp:
        toml_contents = tomllib.load(fp)

    if "step" in toml_contents:
        steps = list(map(make_step, toml_contents["step"]))
    else:
        steps = []

    return Pipeline(pipeline_name, binaries=toml_contents["binaries"], steps=steps)
