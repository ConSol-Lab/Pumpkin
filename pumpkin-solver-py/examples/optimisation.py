from argparse import ArgumentParser
from pathlib import Path

from pumpkin_solver import constraints, Model
from pumpkin_solver.optimisation import OptimisationResult


def main(proof: Path | None):
    model = Model(proof=proof)

    objective = model.new_integer_variable(10, 20, name="objective")

    # .. build the model here

    def on_solution(solution):
        objective_value = solution.int_value(objective)
        print(f"objective_value={objective_value}")

    status = model.optimise(objective, on_solution=on_solution)
    match status:
        case OptimisationResult.Optimal(_solution):
            print("OPTIMAL")

        case OptimisationResult.Satisfiable(_solution):
            print("SATISFIABLE")

        case OptimisationResult.Unsatisfiable():
            print("UNSATISFIABLE")

        case OptimisationResult.Unknown():
            print("UNKNOWN")


if __name__ == "__main__":
    arg_parser = ArgumentParser()

    arg_parser.add_argument("--proof", type=Path, help="The proof file.", default=None)

    args = arg_parser.parse_args()

    main(args.proof)
