#!/bin/bash

set -eux

rustup component add clippy
cargo clippy --all-targets
