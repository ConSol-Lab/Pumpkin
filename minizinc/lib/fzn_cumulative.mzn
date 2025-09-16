% Taken from https://github.com/chuffed/chuffed/blob/develop/chuffed/flatzinc/mznlib/fzn_cumulative.mzn
% See the constraint at https://www.minizinc.org/doc-2.7.6/en/lib-globals-scheduling.html#mzn-ref-globals-scheduling-cumulative


predicate fzn_cumulative(array[int] of var int: s,
                         array[int] of var int: d,
                         array[int] of var int: r,
                         var int: b) =
    assert(
        index_set(s) == index_set(d) /\ index_set(s) == index_set(r),
        "cumulative: the 3 array arguments must have identical index sets",
        forall(i in index_set(d))(d[i] >= 0 /\ r[i] >= 0)
	/\  if is_fixed(d) /\ is_fixed(r) /\ is_fixed(b) then
		    pumpkin_cumulative(s, fix(d), fix(r), fix(b))
	    else
            pumpkin_cumulative_vars(s, d, r, b)
	    endif
    );

predicate pumpkin_cumulative(array[int] of var int: s, array[int] of int: d, array[int] of int: r, int: b);
predicate pumpkin_cumulative_vars(array[int] of var int: s, array[int] of var int: d, array[int] of var int: r, var int: b);
