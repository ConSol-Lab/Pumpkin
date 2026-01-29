import logging

from constraint_solving.config import Config


def common_init() -> Config:
    """Initialization required for all commands."""

    logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
    return Config()
