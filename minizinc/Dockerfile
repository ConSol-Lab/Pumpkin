# A Dockerfile used to submit Pumpkin to the MiniZinc challenge 2024.

# As suggested in the challenge instruction, the docker file contains two 
# stages. The first stage builds the solver from source, and the second
# sets up the MiniZinc configuration to make Pumpkin the default solver.


FROM rust:latest AS builder

# Copy the project source to the Docker image.
COPY . /pumpkin-src

WORKDIR /pumpkin-src

# Compile the solver with optimizations turned on.
RUN cargo build -p pumpkin-cli --release

# Create the MiniZinc image
FROM minizinc/mznc2024:latest

# Copy the solver executable 
COPY --from=builder /pumpkin-src/target/release/pumpkin-cli /pumpkin/pumpkin-cli

# Copy the MiniZinc library
COPY --from=builder /pumpkin-src/minizinc/ /pumpkin/

# Change the path to the solver executable to be the correct one.
RUN sed -i 's/..\/target\/release\/pumpkin-cli/pumpkin-cli/' /pumpkin/pumpkin.msc

# Add Pumpkin to the MiniZinc search path and set it as the default solver.
#
# See https://www.minizinc.org/doc-2.8.5/en/command_line.html#user-configuration-files
RUN echo '{"mzn_solver_path": ["/pumpkin"],' > $HOME/.minizinc/Preferences.json 
RUN echo '"tagDefaults": [["", "pumpkin"]]}' >> $HOME/.minizinc/Preferences.json
