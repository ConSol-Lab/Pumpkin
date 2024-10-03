include "fzn_cumulative.mzn";

predicate fzn_disjunctive(array[int] of var int: s,
                          array[int] of var int: d) =
        forall(i in index_set(d))(d[i] >= 0)
    /\  if is_fixed(d) then
            pumpkin_cumulative(s, fix(d), [1 | i in index_set(s)], 1)
        else
            forall(i, j in index_set(d) where i < j) (
              s[i] + d[i] <= s[j] \/ s[j] + d[j] <= s[i]
            )
        endif
