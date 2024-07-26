#!/bin/bash

set -eux

if [ "$1" = "--pre-commit" ]; then
    # We don't install/update nightly in pre-commit unless there isn't one. If this command fails then please update clippy (e.g. by using `rustup update`).
    if ! rustup toolchain list | grep --silent 'nightly-[a-z]'; then
        rustup toolchain install --no-self-update --profile minimal --component clippy -- nightly
    fi
else
    rustup toolchain install --no-self-update --profile minimal --component clippy -- nightly
fi

cargo +nightly clippy --all-targets -- -Dwarnings
