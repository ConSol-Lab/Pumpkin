from pumpkin_solver import Model, constraints


def test_boolean_integer_conversion():
    # Build the following model:
    #     ~aux -> ~a
    #     ~aux -> ~b
    #     ~aux -> ~c
    #     p -> ~aux \/ ~p
    # Satisfiable, valid solution would be all variables False.

    solver = Model()
    a, b, c, p = [solver.new_boolean_variable(name) for name in "abcp"]
    aux = solver.new_boolean_variable("aux")

    for var in [a, b, c]:
        solver.add_implication(
            constraints.Clause([var.negate()], solver.new_constraint_tag()),
            aux.negate(),
        )

    solver.add_implication(
        constraints.Clause([aux.negate(), p.negate()], solver.new_constraint_tag()), p
    )

    res = solver.satisfy()
    solution = res._0

    for var in [a, b, c, p, aux]:  # bug gets triggered here
        _ = solution.bool_value(var)
