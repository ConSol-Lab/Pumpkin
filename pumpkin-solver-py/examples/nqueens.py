from argparse import ArgumentParser
from pathlib import Path

from pumpkin_solver import constraints, SatisfactionResult, Model


def main(n: int, proof: Path | None):
    assert n > 0, "Please provide a positive non-zero 'n'"

    model = Model(proof=proof)

    variables = [model.new_integer_variable(0, n - 1, name=f"q{i}") for i in range(n)]

    model.add_constraint(
        constraints.AllDifferent(variables, model.new_constraint_tag())
    )

    diag1 = [var.offset(i) for (i, var) in enumerate(variables)]
    diag2 = [var.offset(-i) for (i, var) in enumerate(variables)]

    model.add_constraint(constraints.AllDifferent(diag1, model.new_constraint_tag()))
    model.add_constraint(constraints.AllDifferent(diag2, model.new_constraint_tag()))

    status = model.satisfy()
    match status:
        case SatisfactionResult.Satisfiable(solution):
            row_separator = "+---" * n + "+"

            for row in range(n):
                print(f"{row_separator}")
                queen_col = solution.int_value(variables[row])

                for col in range(n):
                    string = "| * " if queen_col == col else "|   "
                    print(f"{string}", end="")

                print("|")

            print(f"{row_separator}")

        case SatisfactionResult.Unsatisfiable():
            print(f"{n}-queens is unsatisfiable.")

        case SatisfactionResult.Unknown():
            print("Timeout.")


if __name__ == "__main__":
    arg_parser = ArgumentParser()

    arg_parser.add_argument("--proof", type=Path, help="The proof file.", default=None)
    arg_parser.add_argument("n", type=int, help="The size of the chessboard.")

    args = arg_parser.parse_args()

    main(args.n, args.proof)
