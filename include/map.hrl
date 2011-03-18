-ifndef (ESTDCOLL_MAP_HRL).
-define (ESTDCOLL_MAP_HRL, true).

-include_lib ("estdcoll/include/collection.hrl").

-ifdef (MAP_BEHAVIOR_EXPORTS).

-export ([map_values/1,
          put/2]).

-endif.

-ifdef (MAP_BEHAVIOR_TYPES).

-type map() :: module().
-type map_values_fun() :: fun(({any(), any()}) -> any()).

-endif.

-ifdef (MAP_BEHAVIOR_SPECS).

-spec put (any(), any()) -> b_map:map().
-spec map_values (b_map:map_values_fun()) -> b_map:map().

-endif.

-endif.