predicate bool_clause_reif(array [int] of var bool: p, array [int] of var bool: n, var bool: c) =
  c =
    (
      sum (i in index_set(p)) (bool2int(p[i])) -
        sum (i in index_set(n)) (bool2int(n[i])) +
        length(n) >=
        1
    );

predicate array_int_minimum(var int: m, array [int] of var int: x) =
  array_int_minimum_I(m, [x[i] | i in index_set(x)]);
predicate array_int_maximum(var int: m, array [int] of var int: x) =
  array_int_minimum_I(-m, [-x[i] | i in index_set(x)]);
predicate array_float_minimum(var float: m, array [int] of var float: x) =
  array_float_minimum_I(m, [x[i] | i in index_set(x)]);
predicate array_float_maximum(var float: m, array [int] of var float: x) =
  array_float_minimum_I(-m, [-x[i] | i in index_set(x)]);

predicate int_lin_eq_imp(array [int] of int: as, array [int] of var int: bs,
                         int: c, var bool: r) =
    int_lin_le_imp(as, bs, c, r) /\ int_lin_le_imp([-a | a in as], bs, -c, r);

predicate int_lin_ge_imp(array [int] of int: as, array [int] of var int: bs,
                         int: c, var bool: r);
predicate int_lin_gt_imp(array [int] of int: as, array [int] of var int: bs,
                         int: c, var bool: r);
predicate int_lin_le_imp(array [int] of int: as, array [int] of var int: bs,
                         int: c, var bool: r);
predicate int_lin_lt_imp(array [int] of int: as, array [int] of var int: bs,
                         int: c, var bool: r);

predicate int_lin_ne_imp(array [int] of int: as, array [int] of var int: bs,
                         int: c, var bool: r) =
    r -> (int_lin_le(as, bs, c - 1) \/ int_lin_le([-a | a in as], bs, -c - 1));
