#!/bin/bash
set -Eeuo pipefail
name=$(sed -n -e '/name/{s/.* = *//p}' driver.log)
time=$(sed -n -e '/wall_clock_time/{s/.* = *//p}' driver.log)
ub=$(sed -n -e '/%%%mzn-stat: objective=/{s///p}' run.log | tail -n 1)
lb=$(sed -n -e '/%%%mzn-stat: objectiveBound=/{s///p}' run.log | tail -n 1)
echo "$name,$time,$lb,$ub,"
