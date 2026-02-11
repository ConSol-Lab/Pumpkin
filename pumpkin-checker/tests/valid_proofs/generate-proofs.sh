#!/bin/bash

INSTANCES=pumpkin-checker/tests/proofs

for instance_file in $INSTANCES/*.fzn; do
    proof_file="${file%.fzn}.drcp"
    
    cargo run -p pumpkin-solver --release -- \
        --proof-type full \
        --proof-path $proof_file \
        $instance_file
done
