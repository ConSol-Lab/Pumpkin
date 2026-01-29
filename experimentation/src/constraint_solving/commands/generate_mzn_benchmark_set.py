import logging
from pathlib import Path
from typing import List

import click

from constraint_solving import common_init


@click.command()
@click.argument("benchmark_set_name", type=str)
@click.argument("directories", type=Path, nargs=-1)
def run(benchmark_set_name: str, directories: List[Path]) -> int:
    config = common_init()

    if len(directories) == 0:
        logging.error("Specify at least one directory containing MiniZinc instances.")
        return 1

    benchmark_set_definition_path = config.benchmark_dir / f"{benchmark_set_name}.toml"

    with benchmark_set_definition_path.open("w") as benchmark_set_definition:
        for directory in directories:
            directory = config.benchmark_dir / directory

            if not directory.is_dir():
                logging.error("Provided directories to MiniZinc challenge folder structure")
                return 1

            for family in directory.iterdir():
                if not family.is_dir():
                    continue

                for model_path in family.rglob("*.mzn"):
                    for data_path in family.rglob("*.dzn"):
                        benchmark_key = f"{model_path.stem}_{data_path.stem}".replace(".", "_")

                        model_path_arg = model_path.relative_to(config.benchmark_dir)
                        data_path_arg = data_path.relative_to(config.benchmark_dir)

                        benchmark_set_definition.write(f'{benchmark_key} = ["{model_path_arg}", "{data_path_arg}"]\n')

    return 0
