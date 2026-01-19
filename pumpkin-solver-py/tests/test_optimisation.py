from pumpkin_solver import Model
from pumpkin_solver.optimisation import Direction, OptimisationResult


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


def test_warm_start_with_callback():
    model = Model()

    objective = model.new_integer_variable(1, 5, name="objective")

    first_value = None

    def on_solution(solution):
        nonlocal first_value

        if first_value is None:
            first_value = solution.int_value(objective)

    result = model.optimise(
        objective,
        direction=Direction.Maximise,
        warm_start={objective: 3},
        on_solution=on_solution,
    )

    assert isinstance(result, OptimisationResult.Optimal)

    assert first_value == 3
