% Boolean Constraints

predicate array_bool_element(var int: b, array [int] of bool: as, var bool: c) = 
    array_var_bool_element(b, as, c);

% bool2int(a) <= bool2int(b) <-> (a -> b)
predicate bool_le(var bool: a, var bool: b) = a -> b;

predicate bool_lt(var bool: a, var bool: b) = (not a) /\ b;

predicate bool_or(var bool: a, var bool: b, var bool: r) = array_bool_or([a, b], r);
