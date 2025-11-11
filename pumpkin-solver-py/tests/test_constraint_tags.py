from pumpkin_solver import Model


def test_constraint_tags_are_created_in_order():
    model = Model()

    t1 = model.new_constraint_tag()

    assert int(t1) == 1
