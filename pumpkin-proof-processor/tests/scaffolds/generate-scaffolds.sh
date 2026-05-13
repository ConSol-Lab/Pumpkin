#!/bin/bash

INSTANCES=pumpkin-proof-processor/tests/scaffolds

for instance_file in $INSTANCES/*.fzn; do
    instance_name=$(basename $instance_file .fzn)
    echo "Solving $instance_name"
    proof_file="$INSTANCES/$instance_name.scaffold.drcp"

    cargo run -p pumpkin-solver --release -- \
        --proof-type scaffold \
        --proof-path $proof_file \
        $instance_file
done
