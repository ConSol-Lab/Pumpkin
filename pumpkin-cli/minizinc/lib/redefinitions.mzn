include "nosets.mzn";

% Boolean Constraints

% bool2int(a) <= bool2int(b) <-> (a -> b)
predicate bool_le(var bool: a, var bool: b) = a -> b;

predicate bool_lt(var bool: a, var bool: b) = (not a) /\ b;

predicate bool_or(var bool: a, var bool: b, var bool: r) = array_bool_or([a, b], r);

% Set Constraints

predicate set_in_reif(var int: x, set of int: S, var bool: r) = par_set_in_reif(x, S, r);
predicate par_set_in_reif(var int: x, set of int: S, var bool: r);
