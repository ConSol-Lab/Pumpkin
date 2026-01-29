#!/bin/bash

if [ -z "$1" ]
  then
     echo "Should supply folder to write the .fzn files to"
     exit 1
fi

if [ -z "$2" ]
  then
     echo "Should supply the file path of the benchmark definition"
     exit 1
fi

touch "$2"
> "$2"

for i in $(find ./experiments/outputs -name \*.fzn); do
    DIRPATH="$(dirname "$i")"
    DIRNAME="$(basename "$DIRPATH")"

    cp "$i" $1/"${DIRNAME}.fzn"

    UNPREFIXED="$(echo $1 | sed -r 's|benchmarks/||')"
    echo "${DIRNAME} = [\"$UNPREFIXED/${DIRNAME}.fzn\"]" >> "$2"
done
