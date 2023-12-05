#!/bin/bash

set -eux

cargo install --locked cargo-deny
cargo deny check
