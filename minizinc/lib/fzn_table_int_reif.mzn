predicate pumpkin_table_int_reif(array[int] of var int: x, array[int] of int: t, var bool: b);

predicate fzn_table_int_reif(array[int] of var int: x, array[int, int] of int: t, var bool: b) = pumpkin_table_int(x, array1d(t), b);

