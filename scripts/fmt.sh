#!/bin/bash

set -eux

rustup component add rustfmt
cargo fmt --check 
