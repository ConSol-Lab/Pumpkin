from argparse import ArgumentParser
from pathlib import Path
from typing import Optional

from pumpkin_py import constraints, SatisfactionResult, Solver

def main(n: int, proof: Path | None):
    assert n > 0, "Please provide a positive non-zero 'n'"

    solver = Solver(proof=proof)

    variables = [solver.new_variable(0, n - 1, name=f"q{i}") for i in range(n)]

    solver.post(constraints.all_different(variables))

    diag1 = [var.offset(i) for (i, var) in enumerate(variables)]
    diag2 = [var.offset(-i) for (i, var) in enumerate(variables)]

    solver.post(constraints.all_different(diag1))
    solver.post(constraints.all_different(diag2))

    status = solver.satisfy()
    match status:
        case SatisfactionResult.Satisfiable(solution):
            row_separator = "+---" * n + "+"

            for row in range(n):
                print(f"{row_separator}");

                queen_col = solution.value(variables[row])

                for col in range(n): 
                    string = "| * " if queen_col == col else "|   "
                    print(f"{string}", end='')
                
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
