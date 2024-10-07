from argparse import ArgumentParser
from pathlib import Path

from pumpkin_py import Comparator, constraints, SatisfactionResult, Model

def main():
    model = Model()

    xyz = [model.new_integer_variable(0, 5) for _ in range(3)]
    bools = [model.new_boolean_variable(f"bv{i}") for i in range(3)]
    ints = [model.boolean_as_integer(boolean) for boolean in bools]

    model.add_constraint(constraints.LessThanOrEquals(
        [i.scaled(-1) for i in ints],
        -1
    ))

    for (bv, int_var) in zip(bools, xyz):
        model.add_implication(constraints.Equals([int_var], 3), bv)

    while True:
        status = model.satisfy()
        match status:
            case SatisfactionResult.Satisfiable(solution):
                for i, var in enumerate(xyz):
                    print(f"v{i} = {solution.int_value(var)}")
                print("---")

                model.add_constraint(constraints.Clause(
                    [model.predicate_as_boolean(var, Comparator.NotEqual, solution.int_value(var)) for var in xyz]
                ))
            
            case SatisfactionResult.Unsatisfiable():
                print(f"Finished enumeration.")
                break
            
            case SatisfactionResult.Unknown():
                print("Timeout.")
                break


if __name__ == "__main__":
    main()
