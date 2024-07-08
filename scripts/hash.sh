#!/bin/bash

set -ux

results=$(grep --recursive --files-with-matches --exclude="hash_structures.rs" --fixed-strings 'std::collections::Hash' ./pumpkin-lib/)
num_results=$(echo "$results" | wc --lines)
if [ "$results" != "" ]; then
    echo -e "\033[0;31mHash-based structures from the standard library found in file(s):\n$results\033[0m"
    echo -e "\033[0;31mPlease do not used hash-based structures from the standard library, hash-based structures in Rust do not provide any guarantees on the order which leads to poor reproducibility (see /pumpkin-lib/src/basic_types/hash_structures.rs)\033[0m"
    exit 1
else
    exit 0
fi