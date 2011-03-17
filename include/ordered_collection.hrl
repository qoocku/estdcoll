-ifndef (ESTDCOLL_ORDERED_COLLECTION_HRL).
-define (ESTDCOLL_ORDERED_COLLECTION_HRL, true).

-include_lib ("estdcoll/include/collection.hrl").
-include_lib ("estdcoll/include/random_access_collection.hrl").

-ifdef (ORDERED_COLLECTION_BEHAVIOR_EXPORTS).

-export ([append   /1,
          foldl    /2,
          foldr    /2,
          prepend  /1,
          reverse  /0]).

-endif.

-ifdef (ORDERED_COLLECTION_BEHAVIOR_SPECS).

-spec append (any()) -> b_collection:collection().
-spec foldl  (b_collection:fold_fun(),
               b_collection:acc()) -> b_collection:acc().
-spec foldr  (b_collection:fold_fun(),
               b_collection:acc()) -> b_collection:acc().
-spec prepend (any()) -> b_collection:collection().
-spec reverse () -> b_collection:collection().

-endif.

-endif.