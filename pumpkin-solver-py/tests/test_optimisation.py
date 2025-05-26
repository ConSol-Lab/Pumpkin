from pumpkin_solver_py import Model
from pumpkin_solver_py.optimisation import Direction, OptimisationResult


def test_linear_sat_unsat_minimisation():
    model = Model()

    objective = model.new_integer_variable(1, 5, name="objective")

    result = model.optimise(objective, direction=Direction.Minimise)

    assert isinstance(result, OptimisationResult.Optimal)

    solution = result._0
    assert solution.int_value(objective) == 1


def test_linear_sat_unsat_maximisation():
    model = Model()

    objective = model.new_integer_variable(1, 5, name="objective")

    result = model.optimise(objective, direction=Direction.Maximise)

    assert isinstance(result, OptimisationResult.Optimal)

    solution = result._0
    assert solution.int_value(objective) == 5
