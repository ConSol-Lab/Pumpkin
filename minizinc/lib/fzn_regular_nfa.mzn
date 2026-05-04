
predicate pumpkin_regular_nfa(array[int] of var int: x, int: Q, int: S,
                          array[int] of set of int: d, int: q0, set of int: F);

predicate fzn_regular_nfa(array[int] of var int: x, int: Q, int: S,
                      array[int,int] of set of int: d, int: q0, set of int: F) =
  pumpkin_regular_nfa(x,Q,S,array1d(d),q0,F);
