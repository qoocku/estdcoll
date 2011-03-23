-ifndef (ESTDCOLL_SET_HRL).
-define (ESTDCOLL_SET_HRL, true).

-include_lib ("estdcoll/include/collection.hrl").

-ifdef (SET_BEHAVIOR_EXPORTS).

-export ([intersection/1,
          is_disjoint/1,
          subtract/1,
          union/1]).

-endif.

-ifdef (SET_BEHAVIOR_TYPES).

-opaque set_() :: module().
-opaque iterator() :: module().

-endif.

-ifdef (SET_BEHAVIOR_SPECS).

-spec union(b_collection:iterator() | b_set:iterator() | b_collection:collection()) -> b_set:set_().
-spec intersection(b_collection:iterator() | b_set:iterator() | b_collection:collection()) -> b_set:set_().
-spec is_disjoint(b_collection:iterator() | b_set:iterator() | b_collection:collection()) -> boolean().
-spec subtract(b_collection:iterator() | b_set:iterator() | b_collection:collection()) -> b_set:set_().

-endif.

-endif.
