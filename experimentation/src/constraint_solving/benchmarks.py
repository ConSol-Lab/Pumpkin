from dataclasses import dataclass
from pathlib import Path
import tomllib
from typing import Dict, List

from constraint_solving.config import Config


@dataclass
class Benchmark:
    key: str
    arguments: List[str]


@dataclass
class BenchmarkSet:
    benchmarks: List[Benchmark]

    def __iter__(self):
        return iter(self.benchmarks)


class BenchmarkSetDoesNotExist(Exception):
    pass


def load_benchmark_set(config: Config, benchmark_set_name: str) -> BenchmarkSet:
    benchmark_set_path: Path = config.benchmark_dir / f"{benchmark_set_name}.toml"

    if not benchmark_set_path.is_file():
        raise BenchmarkSetDoesNotExist

    with benchmark_set_path.open("rb") as fp:
        toml_contents = tomllib.load(fp)

    benchmarks = list(
        map(lambda item: Benchmark(item[0], item[1]), toml_contents.items())
    )

    return BenchmarkSet(benchmarks)
