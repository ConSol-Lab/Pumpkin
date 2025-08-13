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

% Big-M reformulation of reified constraints
predicate int_lin_le_reif(array [int] of int: as,
                          array [int] of var int: bs,
                          int: c,
                          var bool: r) =
    let {
        set of int: I = index_set(as),
        int: Mtrue = sum(i in I)(
            if as[i] > 0 then as[i] * ub(bs[i])
            else as[i] * lb(bs[i])
            endif
        ) - c,
        int: Mfalse = c + 1 - sum(i in I)(
            if as[i] > 0 then as[i] * lb(bs[i])
            else as[i] * ub(bs[i])
            endif
        )
    } in
    (if Mtrue > 0 then int_lin_le([-Mtrue] ++ as, [bool2int(not r)] ++ bs, c) else r endif) /\
    (if Mfalse > 0 then int_lin_le([-Mfalse] ++ [-a | a in as], [bool2int(r)] ++ bs, -c - 1) else not r endif);

predicate int_lin_ne_reif(array [int] of int: as,
                          array [int] of var int: bs,
                          int: c,
                          var bool: r) = int_lin_eq_reif(as, bs, c, not r);

predicate int_lin_eq_reif(array [int] of int: as,
                          array [int] of var int: bs,
                          int: c,
                          var bool: r) =
    let {
        set of int: I = index_set(as),
        int: Mtrue = sum(i in I)(
            if as[i] > 0 then as[i] * ub(bs[i])
            else as[i] * lb(bs[i])
            endif
        ) - c,
        int: Mfalse = c - sum(i in I)(
            if as[i] > 0 then as[i] * lb(bs[i])
            else as[i] * ub(bs[i])
            endif
        )
    } in
    if Mtrue >= 0 /\ Mfalse >= 0
    then
      (-Mfalse * bool2int(not r) <= sum(i in I)(as[i] * bs[i]) - c) /\
      (sum(i in I)(as[i] * bs[i]) - c <= Mtrue * bool2int(not r)) /\
      (not r -> sum(i in I)(as[i] * bs[i]) != c)
    else not r
    endif;


% Redefinition taken from chuffed:
% https://github.com/chuffed/chuffed/blob/7b3cd017f17f7a4091a5b2569dc4f1a18f07c121/chuffed/flatzinc/mznlib/redefinitions.mzn#L89C1-L90C19
predicate int_mod(var int: x, var int: y, var int: z) =
  x=(x div y)*y+z;

% Set Constraints
