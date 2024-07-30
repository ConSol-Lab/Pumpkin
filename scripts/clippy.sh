#!/bin/bash

set -eux

rustup component add clippy
cargo +nightly clippy --all-targets -- -Dwarnings
