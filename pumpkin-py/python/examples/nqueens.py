from argparse import ArgumentParser

from pumpkin_py import constraints, SatisfactionResult, Solver

def main(n: int):
    assert n > 0, "Please provide a positive non-zero 'n'"

    solver = Solver()

    variables = [solver.new_variable(0, n - 1) for _ in range(n)]

    solver.post(constraints.all_different(variables))

    diag1 = [var.offset(i) for (i, var) in enumerate(variables)]
    diag2 = [var.offset(-i) for (i, var) in enumerate(variables)]

    solver.post(constraints.all_different(diag1))
    solver.post(constraints.all_different(diag2))

    match solver.satisfy():
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
        
        case SatisfactionResult.Unsatisfiable:
            print(f"{n}-queens is unsatisfiable.")
        
        case SatisfactionResult.Unknown:
            print("Timeout.")


if __name__ == "__main__":
    arg_parser = ArgumentParser()

    arg_parser.add_argument("n", type=int, help="The size of the chessboard.")

    args = arg_parser.parse_args()

    main(args.n)
