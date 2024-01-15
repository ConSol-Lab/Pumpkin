#!/bin/bash

set -eux

rustup toolchain install --no-self-update --profile minimal --component rustfmt -- nightly
cargo +nightly fmt --check 
