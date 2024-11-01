predicate pumpkin_array_int_maximum(var int: m, array[int] of var int: x);
predicate array_int_maximum(var int: m, array[int] of var int: x)
= pumpkin_array_int_maximum(m, x);
