-ifndef (ESTDCOLL_RANDOM_ACCESS_COLLECTION_HRL).
-define (ESTDCOLL_RANDOM_ACCESS_COLLECTION_HRL, true).

-define (COLLECTION_BEHAVIOR_TYPES, true).
-include_lib ("estdcoll/include/collection.hrl").

-ifdef (RANDOM_ACCESS_COLLECTION_BEHAVIOR_EXPORTS).

-export ([at/1]).

-endif.

-ifdef (RANDOM_ACCESS_COLLECTION_BEHAVIOR_SPECS).

-spec at (any()) -> any() | no_return().

-endif.

-endif.