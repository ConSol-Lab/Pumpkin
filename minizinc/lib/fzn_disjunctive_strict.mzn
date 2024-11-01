include "fzn_cumulative.mzn";

predicate fzn_disjunctive_strict(array[int] of var int: s,
                          array[int] of var int: d) =
        forall(i in index_set(d))(d[i] >= 0)
    /\  fzn_cumulative(s, d, [1 | i in index_set(s)], 1)
    ;
