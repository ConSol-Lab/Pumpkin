#!/bin/bash

set -eux

cat "minizinc/pumpkin.msc" | json_pp -t null
cat "minizinc/pumpkin-for-proofs.msc" | json_pp -t null
