-ifndef (ESTDCOLL_STRICT_COLLECTION_HRL).
-define (ESTDCOLL_STRICT_COLLECTION_HRL, true).

-include_lib ("estdcoll/include/collection.hrl").

-ifdef (STRICT_COLLECTION_BEHAVIOR_EXPORTS).

-export ([size/0]).

-endif.

-ifdef (STRICT_COLLECTION_BEHAVIOR_SPECS).

-spec size () -> non_neg_integer().

-endif.

-endif.