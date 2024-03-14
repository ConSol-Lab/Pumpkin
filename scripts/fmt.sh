#!/bin/bash

set -eux

if [ "$1" = "--pre-commit" ]; then
    # We don't install/update nightly in pre-commit unless there isn't one. Update it yourself
    if ! rustup toolchain list | grep --silent 'nightly-[a-z]'; then
        rustup toolchain install --no-self-update --profile minimal --component rustfmt -- nightly
    fi
else
    rustup toolchain install --no-self-update --profile minimal --component rustfmt -- nightly
fi
cargo +nightly fmt --check 
