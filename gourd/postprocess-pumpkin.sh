#!/bin/bash
set -Eeuo pipefail
name=$(sed -n -e '/name/{s/.* = *//p}' driver.log)
time=$(sed -n -e '/wall_clock_time/{s/.* = *//p}' driver.log)
error_code=$(if [ -f driver.err ]; then sed -n -e '/exit_code/{s/.* = *//p}' driver.err; else echo 0; fi)
makespan=$(sed -n -e '/%%%mzn-stat: objective=/{s///p}' run.log | tail -n 1)
is_optimal=false;
if grep 'Found optimal solution' run.log > /dev/null; then
  is_optimal=true;
fi
stats=$(python /home/ksidorov/RustRoverProjects/Pumpkin/gourd/process_stats.py run.log)
echo "$name,$time,$error_code,$makespan,$is_optimal,$stats"
