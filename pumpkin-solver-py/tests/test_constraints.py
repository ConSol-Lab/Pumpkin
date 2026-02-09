"""
Generate constraints and expressions based on the grammar supported by the API

Generates linear constraints, special operators and global constraints.
Whenever possible, the script also generates 'boolean as integer' versions of the arguments
"""

from random import randint

import pytest
from pumpkin_solver import constraints
import pumpkin_solver


def chain(*iterables):
    """Helper used to chain iterables/generators."""
    for it in iterables:
        for element in it:
            yield element


# generate all linear sum-expressions
def generate_linear():
    for comp in "<=", "==", "!=":
        for scaled in (False, True):  # to generate a weighted sum
            for bool in (False, True):  # from bool-view?
                yield comp, scaled, bool


# generate other operators
def generate_operators():
    for name in ["div", "mul", "abs", "min", "max", "element"]:
        for scaled in (False, True):
            for bool in (False, True):  # from bool-view?
                yield name, scaled, bool


# generate global constraints, separate functions for readability
def generate_alldiff():
    for scaled in (False, True):
        for bool in (False, True):  # from bool-view? Unlikely constraint, but anyway
            yield "alldiff", scaled, bool


def generate_table():
    yield "table", False, False


def generate_negative_table():
    yield "negative_table", False, False


def generate_cumulative():
    yield "cumulative", False, False
    yield "cumulative", True, False


def generate_globals():
    yield from generate_alldiff()
    yield from generate_cumulative()
    yield from generate_table()
    yield from generate_negative_table()


def create_linear_model(request):
    comp, scaled, bool = request.param

    model = pumpkin_solver.Model()

    if bool:
        args = [
            model.new_boolean_variable(name=f"x[{i}]").as_integer() for i in range(3)
        ]
    else:
        args = [model.new_integer_variable(-3, 5, name=f"x[{i}]") for i in range(3)]
    if scaled:  # do scaling (0, -2, 4,...)
        args = [
            a.scaled(-2 * i + 1) for i, a in enumerate(args)
        ]  # TODO: div by zero when scale = 0, fixed with +1

    rhs = 1
    cons = None
    if comp == "==":
        cons = constraints.Equals(args, rhs, model.new_constraint_tag())
    if comp == "!=":
        cons = constraints.NotEquals(args, rhs, model.new_constraint_tag())
    if comp == "<=":
        cons = constraints.LessThanOrEquals(args, rhs, model.new_constraint_tag())

    if not cons:
        assert False, f"unknown comp {comp}"

    return (model, cons)


@pytest.fixture
def linear_model(request):
    return create_linear_model(request)


def create_operator_model(request):
    name, scaled, bool = request.param

    model = pumpkin_solver.Model()

    if bool:
        args = [
            model.new_boolean_variable(name=f"x[{i}]").as_integer() for i in range(3)
        ]
    else:
        args = [model.new_integer_variable(-3, 5, name=f"x[{i}]") for i in range(3)]
    if scaled:  # do scaling (0, -2, 4,...)
        args = [
            a.scaled(-2 * i + 1) for i, a in enumerate(args)
        ]  # TODO: div by zero when scale = 0, fixed with +1

    rhs = model.new_integer_variable(-3, 5, name="rhs")
    cons = None
    if name == "div":
        denom = model.new_integer_variable(1, 3, name="denom")
        cons = constraints.Division(args[0], denom, rhs, model.new_constraint_tag())
    if name == "mul":
        cons = constraints.Times(*args[:2], rhs, model.new_constraint_tag())
    if name == "abs":
        cons = constraints.Absolute(args[0], rhs, model.new_constraint_tag())
    if name == "min":
        cons = constraints.Minimum(args, rhs, model.new_constraint_tag())
    if name == "max":
        cons = constraints.Maximum(args, rhs, model.new_constraint_tag())
    if name == "element":
        idx = model.new_integer_variable(
            -1, 5, name="idx"
        )  # sneaky, idx can be out of bounds
        cons = constraints.Element(idx, args, rhs, model.new_constraint_tag())

    if not cons:
        assert False, f"unknown constraint {name}"

    return (model, cons)


@pytest.fixture
def operator_model(request):
    return create_operator_model(request)


def create_global_model(request):
    name, scaled, bool = request.param

    model = pumpkin_solver.Model()

    if name == "alldiff":
        if bool:
            args = [
                model.new_boolean_variable(name=f"x[{i}]").as_integer()
                for i in range(3)
            ]
        else:
            args = [model.new_integer_variable(-3, 5, name=f"x[{i}]") for i in range(3)]
        if scaled or bool:  # do scaling (0, -2, 4,...)
            args = [
                a.scaled(-2 * i + 1) for i, a in enumerate(args)
            ]  # TODO: div by zero when scale = 0, fixed with +1

        cons = constraints.AllDifferent(args, model.new_constraint_tag())

    elif name == "table":
        variables = [model.new_integer_variable(1, 5, name=f"x[{i}]") for i in range(3)]
        table = [[randint(1, 5) for _ in range(3)] for _ in range(3)]
        cons = constraints.Table(variables, table, model.new_constraint_tag())

    elif name == "negative_table":
        variables = [model.new_integer_variable(1, 5, name=f"x[{i}]") for i in range(3)]
        table = [[randint(1, 5) for _ in range(3)] for _ in range(3)]
        cons = constraints.NegativeTable(variables, table, model.new_constraint_tag())

    elif name == "cumulative":
        duration = [2, 3, 4]
        demand = [1, 2, 3]
        capacity = 4

        start = [model.new_integer_variable(-3, 5, name=f"x[{i}]") for i in range(3)]
        if scaled:
            start = [a.scaled(-2 * i) for i, a in enumerate(start)]
        cons = constraints.Cumulative(
            start, duration, demand, capacity, model.new_constraint_tag()
        )

    else:
        assert False, f"unknown global {name}"

    return (model, cons)


@pytest.fixture
def global_model(request):
    return create_global_model(request)


def make_id(args):
    name, scaled, bool = args

    return " ".join(
        ["Scaled" if scaled else "Unscaled", "Boolean" if bool else "Integer", name]
    )


@pytest.mark.parametrize("linear_model", generate_linear(), indirect=True, ids=make_id)
def test_linear(linear_model):
    model, cons = linear_model
    model.add_constraint(cons)

    res = model.satisfy()
    assert isinstance(res, pumpkin_solver.SatisfactionResult.Satisfiable)


@pytest.mark.parametrize(
    "operator_model",
    generate_operators(),
    ids=make_id,
    indirect=True,
)
def test_operators(operator_model):
    model, cons = operator_model
    model.add_constraint(cons)

    res = model.satisfy()
    assert isinstance(res, pumpkin_solver.SatisfactionResult.Satisfiable)


@pytest.mark.parametrize(
    "global_model",
    generate_globals(),
    ids=make_id,
    indirect=True,
)
def test_globals(global_model):
    model, cons = global_model
    model.add_constraint(cons)

    res = model.satisfy()
    assert isinstance(res, pumpkin_solver.SatisfactionResult.Satisfiable)


@pytest.fixture
def implication_model(request):
    name, _, _ = request.param

    if name in ["<=", "==", "!="]:
        model, cons = create_linear_model(request)
    elif name in ["div", "mul", "abs", "min", "max", "element"]:
        model, cons = create_operator_model(request)
    elif name in ["alldiff", "element", "cumulative", "table", "negative_table"]:
        model, cons = create_global_model(request)

    else:
        assert False, f"unknown global {name}"

    return model, cons


@pytest.mark.parametrize(
    "implication_model",
    chain(generate_linear(), generate_operators(), generate_globals()),
    ids=make_id,
    indirect=True,
)
def test_implication(implication_model):
    model, cons = implication_model

    bv = model.new_boolean_variable("bv")
    model.add_implication(cons, bv)

    res = model.satisfy()
    assert isinstance(res, pumpkin_solver.SatisfactionResult.Satisfiable)
