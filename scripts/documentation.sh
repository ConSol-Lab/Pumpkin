#!/bin/bash

set -eux

rustup component add rust-docs
RUSTDOCFLAGS="-D warnings" cargo doc --no-deps