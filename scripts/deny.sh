#!/bin/bash

set -eux

if [ "$1" = "--pre-commit" ]; then
    # We don't install a new cargo-deny in pre-commit unless there isn't one. Update it yourself
    cargo deny -V || cargo install cargo-deny
else
    # We ignore errors from install, specifically for the error when it's already up-to-date
    cargo install cargo-deny || true
fi
cargo deny check
