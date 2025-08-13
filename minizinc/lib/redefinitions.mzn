include "nosets.mzn";

% Boolean Constraints

% bool2int(a) <= bool2int(b) <-> (a -> b)
predicate bool_le(var bool: a, var bool: b) = a -> b;

predicate bool_lt(var bool: a, var bool: b) = (not a) /\ b;

predicate bool_or(var bool: a, var bool: b, var bool: r) = array_bool_or([a, b], r);

% Decomposition borrowed from Chuffed: 
% https://github.com/chuffed/chuffed/blob/5ecfa5476afd01e2f18fbbbbcc611ce0de7d351f/chuffed/flatzinc/mznlib/redefinitions.mzn#L69C1-L88C1
predicate array_bool_xor(array[int] of var bool: bs) =
    let { int: bs_lower = min(index_set(bs)),
          int: bs_upper = max(index_set(bs)),
          int: n = length(bs)
    } in
        if n == 1 then bs[bs_lower] else
        if n == 2 then bs[bs_lower] xor bs[bs_upper] else
        if n == 3 then bs[bs_lower] = (bs[bs_lower + 1] = bs[bs_upper])
        else
            let { int: cs_lower = bs_lower + 1,
                  int: cs_upper = bs_upper - 1,
                  array [cs_lower..cs_upper] of var bool: cs
            } in
                forall(i in cs_lower..cs_upper-1)(
                    cs[i+1] = bs[i+1] xor cs[i]
                )
                /\ (cs[cs_lower] = bs[bs_lower] xor bs[bs_lower + 1])
                /\ (bs[bs_upper] xor cs[cs_upper])
        endif endif endif;

predicate bool_xor(var bool: a, var bool: b) = pumpkin_bool_xor(a, b);
predicate bool_xor(var bool: a, var bool: b, var bool: r) = pumpkin_bool_xor_reif(a, b, r);
predicate pumpkin_bool_xor(var bool: a, var bool: b);
predicate pumpkin_bool_xor_reif(var bool: a, var bool: b, var bool: r);

% Redefinition taken from chuffed:
% https://github.com/chuffed/chuffed/blob/7b3cd017f17f7a4091a5b2569dc4f1a18f07c121/chuffed/flatzinc/mznlib/redefinitions.mzn#L89C1-L90C19
predicate int_mod(var int: x, var int: y, var int: z) =
  x=(x div y)*y+z;

% Set Constraints

%%% NOTE: because of a mistake in the MiniZinc standard library, the
% `warm_start` annotation is overloaded on the FlatZinc level, the following
% definitions disambiguate these annotations within Chuffed.

annotation warm_start_bool(array[int] of var bool: x, array[int] of bool: v);
annotation warm_start(array[int] of var bool: x, array[int] of bool: v) = warm_start_bool(x, v);
annotation warm_start_int(array[int] of var int: x, array[int] of int: v);
annotation warm_start(array[int] of var int: x, array[int] of int: v) = warm_start_int(x, v);
annotation warm_start_float(array[int] of var float: x, array[int] of float: v);
annotation warm_start(array[int] of var float: x, array[int] of float: v) = warm_start_float(x, v);
