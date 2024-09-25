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
            fzn_decomposition_cumulative(s, d, r, b)
	    endif
    );

% The following predicate is taken from https://github.com/MiniZinc/libminizinc/blob/2.5.5/share/minizinc/std/cumulative.mzn
predicate fzn_decomposition_cumulative(array[int] of var int: s,
                         array[int] of var int: d,
                         array[int] of var int: r,
                         var int: b) =
                         		        assert(index_set(s) == index_set(d) /\ index_set(s) == index_set(r),
                    "cumulative: the 3 array arguments must have identical index sets",
                if length(s) >= 1 then
                assert(lb_array(d) >= 0 /\ lb_array(r) >= 0,
                    "cumulative: durations and resource usages must be non-negative",
                    if  
                    %% the previous test will not work for resources usages like [2,3,3,4,4] with a bound of 4 since the 2 appears exactly once
                    let { int: mr = lb_array(r); 
                            int: mri = arg_min([ lb(r[i]) | i in index_set(r) ]) } in
                    forall(i in index_set(r))
                            (is_fixed(r[i]) /\ (fix(r[i]) + mr > ub(b) \/ i = mri))
                    then
                        if forall(i in index_set(d))(is_fixed(d[i]) /\ fix(d[i]) == 1)
                        then
                            all_different(s)
                        else
                            disjunctive(s, d)
                        endif
                    else
                        decomposition_cumulative(s, d, r, b)
                    endif
                )
                endif);

% The following predicate is taken from https://github.com/MiniZinc/libminizinc/blob/2.5.5/share/minizinc/std/fzn_cumulative.mzn
predicate decomposition_cumulative(array[int] of var int: s,
                     array[int] of var int: d,
                     array[int] of var int: r, var int: b) =
        let { 
            set of int: Tasks = 
                {i | i in index_set(s) where ub(r[i]) > 0 /\ ub(d[i]) > 0 }
        } in
            if 0==card(Tasks) then /*true*/ 0==card(index_set(s)) \/ b>=0
            else
                let {
                    int: early = min([ lb(s[i])            | i in Tasks ]), 
                    int: late  = max([ ub(s[i]) + ub(d[i]) | i in Tasks ]) 
                } in (
                    if late - early > 5000 then
                      fzn_cumulative_task(s, d, r, b)
                    else
                      fzn_cumulative_time(s, d, r, b)
                    endif
                )
            endif
            ;

% The following predicate is taken from https://github.com/MiniZinc/libminizinc/blob/2.5.5/share/minizinc/std/fzn_cumulative.mzn
predicate fzn_cumulative_time(array[int] of var int: s,
                     array[int] of var int: d,
                     array[int] of var int: r, var int: b) =
    let { 
        set of int: Tasks = 
            {i | i in index_set(s) where ub(r[i]) > 0 /\ ub(d[i]) > 0 },
        int: early = min([ lb(s[i]) | i in Tasks ]), 
        int: late  = max([ ub(s[i]) + ub(d[i]) | i in Tasks ]) 
    } in (
        forall( t in early..late ) (
            b >= sum( i in Tasks ) (
                bool2int(s[i] <= t /\ t < s[i] + d[i]) * r[i]
            )
        )
    );

% The following predicate is taken from https://github.com/MiniZinc/libminizinc/blob/2.5.5/share/minizinc/std/fzn_cumulative.mzn
predicate fzn_cumulative_task(array[int] of var int: s,
                     array[int] of var int: d,
                     array[int] of var int: r, var int: b) =
    let { 
        set of int: Tasks = 
            {i | i in index_set(s) where ub(r[i]) > 0 /\ ub(d[i]) > 0 }
    } in (
        forall( j in Tasks ) ( 
            b >= r[j] + sum( i in Tasks where i != j ) ( 
                bool2int(s[i] <= s[j] /\ s[j] < s[i] + d[i] ) * r[i]
            )
        )
    );

predicate pumpkin_cumulative(array[int] of var int: s, array[int] of int: d, array[int] of int: r, int: b);
predicate pumpkin_cumulative_vars(array[int] of var int: s, array[int] of var int: d, array[int] of var int: r, var int: b);
