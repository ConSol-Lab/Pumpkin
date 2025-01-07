from pumpkin_py import Model, constraints, Comparator

model = Model()

iv = model.new_integer_variable(0,1)
a = model.new_boolean_variable(name="a")
b = model.new_boolean_variable(name="b")
c = model.new_boolean_variable(name="c")

model.add_constraint(constraints.Clause([a, b]))          # a \/ b
model.add_constraint(constraints.Clause([b.negate(),c]))  # b -> c
model.add_constraint(constraints.Clause([model.predicate_as_boolean(iv, Comparator.Equal, 1)])) # iv == 1
model.add_implication(constraints.Equals([iv], 0), c)  # c -> iv == 0

model.satisfy(proof="test")
