#!/bin/bash
set -Eeuo pipefail
name=$(sed -n -e '/name/{s/.* = *//p}' driver.log)
time=$(sed -n -e '/wall_clock_time/{s/.* = *//p}' driver.log)
ub=$(sed -n -e '/.*Best plan has makespan /{s///p}' run.err)
lb=$(sed -n -e '/.*Makespan lower bound is /{s///p}' run.err)
num_nodes=$(sed -n -e '/.*Explored tree size is /{s///p}' run.err)
echo "$name,$time,$lb,$ub,$num_nodes"
