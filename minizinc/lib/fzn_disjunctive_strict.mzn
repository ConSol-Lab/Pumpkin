% Taken from https://github.com/chuffed/chuffed/blob/develop/chuffed/flatzinc/mznlib/fzn_disjunctive_strict.mzn
%------------------------------------------------------------------------------%
% Requires that a set of tasks given by start times s and durations d
% do not overlap in time. Tasks with duration 0 CANNOT be scheduled at any time,
% but only when no other task is running.
%
%  Assumptions:
%  - forall i, d[i] >= 0
%------------------------------------------------------------------------------%

predicate fzn_disjunctive_strict(array[int] of var int: s,
                                 array[int] of var int: d) =
        forall(i in index_set(d))(d[i] >= 0)
    /\  if is_fixed(d) then
            pumpkin_disjunctive_strict(s, fix(d))
        else
            forall(i, j in index_set(d) where i < j) (
              s[i] + d[i] <= s[j] \/ s[j] + d[j] <= s[i]
            )
        endif
    ;

    % Global disjunctive propagator
    %
predicate pumpkin_disjunctive_strict(array[int] of var int: s, array[int] of int: d);
