from dataclasses import dataclass
from pathlib import Path
from typing import List
import tomllib

from constraint_solving import Config


@dataclass
class Binary:
    name: str
    path: Path


@dataclass
class Source:
    name: str
    git_url: str
    compile_command: str
    binaries: List[Binary]

    def supplies_binary(self, binary_name: str) -> bool:
        return any(binary for binary in self.binaries if binary.name == binary_name)


class BinaryNotInAnySource(Exception):
    binary_name: str

    def __init__(self, binary_name: str, *args: object) -> None:
        super().__init__(*args)

        self.binary_name = binary_name


def make_binary(toml) -> Binary:
    return Binary(
        name=toml["name"],
        path=toml["path"],
    )


def load_source(path: Path) -> Source:
    assert path.is_file()

    with path.open("rb") as fp:
        toml_contents = tomllib.load(fp)

    binaries = list(map(make_binary, toml_contents["binary"]))

    return Source(
        path.stem,
        compile_command=toml_contents["compile"],
        git_url=toml_contents["git-url"],
        binaries=binaries,
    )


def resolve_sources(config: Config, binaries: List[str]) -> List[Source]:
    # Make a copy so removal does not modify the input list.
    binaries = list(binaries)

    sources = []
    for path in config.sources_dir.glob("*.toml"):
        source = load_source(path)

        for binary in binaries:
            if source.supplies_binary(binary):
                sources.append(source)
                binaries.remove(binary)

    if len(binaries) > 0:
        raise BinaryNotInAnySource(binaries[0])

    return sources
