# Pumpkin MiniZinc Files

This directory contains the solver configuration file for Pumpkin, as well as 
the FlatZinc library.


## MiniZinc Challenge

The `Dockerfile` in this directory is used to prepare the docker image for the 
MiniZinc challenge. To create the image, run the following command from the 
project root (aka the Cargo workspace root).

```
$ docker build -t <image>:<tag> -f minizinc/Dockerfile .
```
