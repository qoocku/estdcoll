-ifndef (ESTDCOLL_SET_HRL).
-define (ESTDCOLL_SET_HRL, true).

-include_lib ("estdcoll/include/collection.hrl").

-ifdef (SET_BEHAVIOR_EXPORTS).

-export ([merge/1]).

-endif.

-ifdef (SET_BEHAVIOR_TYPES).

-opaque set_() :: module().
-opaque iterator() :: module().

-endif.

-ifdef (SET_BEHAVIOR_SPECS).

-spec merge(b_collection:iterator() | b_set:iterator() | b_collection:collection()) -> b_set:set_().

-endif.

-endif.
