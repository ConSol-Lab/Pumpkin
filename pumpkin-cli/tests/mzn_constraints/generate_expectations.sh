#!/bin/bash

# Generate all the `.expected` files for the `.fzn` files in this directory. 
# Assumes the current working directory is the directory of the script itself.

for input in ./*.fzn;
do
    test_name=${input%.fzn}
    test_name=${test_name#./}
    echo "Generating expectation for '$test_name'"

    expectation_file=$test_name.expected

    # If there is a $test_name.template.fzn file, use it to generate the 
    # expectation. This means the flatzinc contains a global which has been
    # specialized to pumpkin itself, and thus Gecode will not recognize it.

    if [[ -f $test_name.template ]];
    then
        fzn=$test_name.actual.fzn
        cp $test_name.template $fzn
        echo "  Using template file"
    else
        fzn=$input
    fi

    minizinc --solver gecode -a $fzn > $expectation_file

    if [[ -f $test_name.template ]];
    then
        rm $fzn
    fi
done
