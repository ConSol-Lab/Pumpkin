from pumpkin_solver_py import (
    Comparator,
    Model,
    Predicate,
    SatisfactionUnderAssumptionsResult,
)
from pumpkin_solver_py.constraints import LessThanOrEquals


def test_assumptions_are_respected():
    model = Model()

    x = model.new_integer_variable(1, 5, name="x")

    assumption = Predicate(x, Comparator.LessThanOrEqual, 3)

    result = model.satisfy_under_assumptions([assumption])
    assert isinstance(result, SatisfactionUnderAssumptionsResult.Satisfiable)

    solution = result._0
    x_value = solution.int_value(x)
    assert x_value <= 3


def test_core_extraction():
    model = Model()

    x = model.new_integer_variable(1, 5, name="x")
    y = model.new_integer_variable(1, 5, name="x")

    x_ge_3 = Predicate(x, Comparator.GreaterThanOrEqual, 3)
    y_ge_3 = Predicate(y, Comparator.GreaterThanOrEqual, 3)

    le_tag = model.new_constraint_tag()
    model.add_constraint(LessThanOrEquals([x, y], 5, le_tag))

    result = model.satisfy_under_assumptions([x_ge_3, y_ge_3])
    assert isinstance(
        result, SatisfactionUnderAssumptionsResult.UnsatisfiableUnderAssumptions
    )

    core = set(result._0)
    assert set([x_ge_3, y_ge_3]) == core
