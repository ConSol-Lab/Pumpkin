"""
Generate constraints and expressions based on the grammar supported by the API

Generates linear constraints, special operators and global constraints.
Whenever possible, the script also generates 'boolean as integer' versions of the arguments
"""

from random import randint

import pytest
from pumpkin_py import constraints
import pumpkin_py


# generate all linear sum-expressions
def generate_linear():
    for comp in "<=", "==", "!=":
        for scaled in (False, True):  # to generate a weighted sum
            for bool in (False, True):  # from bool-view?
                model = pumpkin_py.Model()

                if bool:
                    args = [
                        model.boolean_as_integer(
                            model.new_boolean_variable(name=f"x[{i}]")
                        )
                        for i in range(3)
                    ]
                else:
                    args = [
                        model.new_integer_variable(-3, 5, name=f"x[{i}]")
                        for i in range(3)
                    ]
                if scaled:  # do scaling (0, -2, 4,...)
                    args = [
                        a.scaled(-2 * i + 1) for i, a in enumerate(args)
                    ]  # TODO: div by zero when scale = 0, fixed with +1

                rhs = 1
                if comp == "==":
                    cons = constraints.Equals(args, rhs)
                if comp == "!=":
                    cons = constraints.NotEquals(args, rhs)
                if comp == "<=":
                    cons = constraints.LessThanOrEquals(args, rhs)

                yield model, cons, comp, scaled, bool


# generate other operators
def generate_operators():
    for name in ["div", "mul", "abs", "min", "max", "element"]:
        for scaled in (False, True):
            for bool in (False, True):  # from bool-view?
                model = pumpkin_py.Model()

                if bool:
                    args = [
                        model.boolean_as_integer(
                            model.new_boolean_variable(name=f"x[{i}]")
                        )
                        for i in range(3)
                    ]
                else:
                    args = [
                        model.new_integer_variable(-3, 5, name=f"x[{i}]")
                        for i in range(3)
                    ]
                if scaled:  # do scaling (0, -2, 4,...)
                    args = [
                        a.scaled(-2 * i + 1) for i, a in enumerate(args)
                    ]  # TODO: div by zero when scale = 0, fixed with +1

                rhs = model.new_integer_variable(-3, 5, name="rhs")
                if name == "div":
                    denom = model.new_integer_variable(1, 3, name="denom")
                    cons = constraints.Division(args[0], denom, rhs)
                if name == "mul":
                    cons = constraints.Times(*args[:2], rhs)
                if name == "abs":
                    cons = constraints.Absolute(args[0], rhs)
                if name == "min":
                    cons = constraints.Minimum(args, rhs)
                if name == "max":
                    cons = constraints.Maximum(args, rhs)
                if name == "element":
                    idx = model.new_integer_variable(
                        -1, 5, name="idx"
                    )  # sneaky, idx can be out of bounds
                    cons = constraints.Element(idx, args, rhs)

                yield model, cons, name, scaled, bool


# generate global constraints, separate functions for readability
def generate_alldiff():
    for scaled in (False, True):
        for bool in (False, True):  # from bool-view? Unlikely constraint, but anyway
            model = pumpkin_py.Model()
            if bool:
                args = [
                    model.boolean_as_integer(model.new_boolean_variable(name=f"x[{i}]"))
                    for i in range(3)
                ]
            else:
                args = [
                    model.new_integer_variable(-3, 5, name=f"x[{i}]") for i in range(3)
                ]
            if scaled or bool:  # do scaling (0, -2, 4,...)
                args = [
                    a.scaled(-2 * i + 1) for i, a in enumerate(args)
                ]  # TODO: div by zero when scale = 0, fixed with +1

            cons = constraints.AllDifferent(args)
            yield model, cons, "alldifferent", scaled or bool, bool


def generate_table():
    model = pumpkin_py.Model()
    variables = [model.new_integer_variable(1, 5, name=f"x[{i}]") for i in range(3)]

    table = [[randint(1, 5) for _ in range(3)] for _ in range(3)]

    cons = constraints.Table(variables, table)
    yield model, cons, "table", bool, bool


def generate_negative_table():
    model = pumpkin_py.Model()
    variables = [model.new_integer_variable(1, 5, name=f"x[{i}]") for i in range(3)]

    table = [[randint(1, 5) for _ in range(3)] for _ in range(3)]

    cons = constraints.NegativeTable(variables, table)
    yield model, cons, "negative_table", bool, bool


def generate_cumulative():
    duration = [2, 3, 4]
    demand = [1, 2, 3]
    capacity = 4

    model = pumpkin_py.Model()
    start = [model.new_integer_variable(-3, 5, name=f"x[{i}]") for i in range(3)]
    cons = constraints.Cumulative(start, duration, demand, capacity)
    yield model, cons, "cumulative", False, False

    model = pumpkin_py.Model()
    start = [model.new_integer_variable(-3, 5, name=f"x[{i}]") for i in range(3)]
    start = [a.scaled(-2 * i) for i, a in enumerate(start)]
    cons = constraints.Cumulative(start, duration, demand, capacity)
    yield model, cons, "cumulative", True, False


def generate_globals():
    yield from generate_alldiff()
    yield from generate_cumulative()
    yield from generate_table()
    yield from generate_negative_table()


def label(model, cons, name, scaled, bool):
    return " ".join(
        ["Scaled" if scaled else "Unscaled", "Boolean" if bool else "Integer", name]
    )


LINEAR = list(generate_operators())


@pytest.mark.parametrize(
    ("model", "cons", "name", "scaled", "bool"), LINEAR, ids=[label(*a) for a in LINEAR]
)
def test_linear(model, cons, name, scaled, bool):
    model.add_constraint(cons)
    res = model.satisfy()
    assert isinstance(res, pumpkin_py.SatisfactionResult.Satisfiable)


OPERATORS = list(generate_operators())


@pytest.mark.parametrize(
    ("model", "cons", "name", "scaled", "bool"),
    OPERATORS,
    ids=[label(*a) for a in OPERATORS],
)
def test_operators(model, cons, name, scaled, bool):
    model.add_constraint(cons)
    res = model.satisfy()
    assert isinstance(res, pumpkin_py.SatisfactionResult.Satisfiable)


GLOBALS = list(generate_globals())


@pytest.mark.parametrize(
    ("model", "cons", "name", "scaled", "bool"),
    GLOBALS,
    ids=[label(*a) for a in GLOBALS],
)
def test_global(model, cons, name, scaled, bool):
    model.add_constraint(cons)
    res = model.satisfy()
    assert isinstance(res, pumpkin_py.SatisfactionResult.Satisfiable)


ALL_EXPR = (
    list(generate_operators()) + list(generate_linear()) + list(generate_globals())
)


@pytest.mark.parametrize(
    ("model", "cons", "name", "scaled", "bool"),
    ALL_EXPR,
    ids=["->" + label(*a) for a in ALL_EXPR],
)
def test_implication(model, cons, name, scaled, bool):
    if name == "element":
        return  # TODO: propagator not yet implemented?

    bv = model.new_boolean_variable("bv")
    model.add_implication(cons, bv)
    res = model.satisfy()
    assert isinstance(res, pumpkin_py.SatisfactionResult.Satisfiable)
