-ifndef (ESTDCOLL_ITERATOR_HRL).
-define (ESTDCOLL_ITERATOR_HRL, true).

-record (repr, {r :: any()}).
-record (nxt, {pair :: {any(), b_collection:iterator()},
               oper :: fun(),
               next :: {module(), atom()}}).

-endif.
