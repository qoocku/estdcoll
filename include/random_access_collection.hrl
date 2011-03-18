-ifndef (ESTDCOLL_RANDOM_ACCESS_COLLECTION_HRL).
-define (ESTDCOLL_RANDOM_ACCESS_COLLECTION_HRL, true).

-ifdef (RANDOM_ACCESS_COLLECTION_BEHAVIOR_SKELETONS).
-define (USE_SKELETONS, true).
-endif.

-include_lib ("estdcoll/include/collection.hrl").

-ifdef (RANDOM_ACCESS_COLLECTION_BEHAVIOR_EXPORTS).

-export ([at/1]).

-endif.

-ifdef (RANDOM_ACCESS_COLLECTION_BEHAVIOR_SPECS).

-spec at (any()) -> any().

-endif.

-endif.