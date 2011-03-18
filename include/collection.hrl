-ifndef (ESTDCOLL_COLLECTION_HRL).
-define (ESTDCOLL_COLLECTION_HRL, true).

-ifdef (COLLECTION_BEHAVIOR_EXPORTS).

-export ([all      /1,
          any      /1,          
          delete   /1,
          extend   /1,
          filter   /1,      
          fold     /2,
          foreach  /1,
          has      /1,
          internals/0,
          is_empty /0,
          map      /1,
          merge    /2,
          put      /1,
          to_erlang/0]).

-endif.

-ifdef (COLLECTION_BEHAVIOR_TYPES).

-type internals   () :: any().
-type collection  () :: module().
-type acc         () :: any().
-type predicate   () :: fun ((any()) -> boolean()).
-type all_fun     () :: predicate().
-type any_fun     () :: predicate().
-type filter_fun  () :: predicate().
-type fold_fun    () :: fun ((any(), acc()) -> acc()).
-type trav_fun    () :: fun ((any()) -> any()).
-type merge_fun   () :: fun ((any(), any()) -> any()).

-export_type ([acc/0,
               all_fun/0,
               any_fun/0,
               collection/0,
               filter_fun/0,
               fold_fun /0,
               internals/0,
               merge_fun/0,
               predicate/0,
               trav_fun /0]).

-endif.

-ifdef (COLLECTION_BEHAVIOR_SPECS).

%% @doc Returns `true' iff the predicate is true for all of the elements.
-spec all       (b_collection:all_fun()) -> boolean().
%% @doc Returns `true' iff the predicate is true for any of the elements.
-spec any       (b_collection:any_fun()) -> boolean().
-spec delete    (any()) -> b_collection:collection().
-spec extend    (b_collection:collection()) -> b_collection:collection() | no_return().
-spec filter    (b_collection:predicate()) ->  b_collection:collection().
-spec fold      (b_collection:fold_fun(),
                  b_collection:acc()) -> b_collection:acc().
-spec foreach   (b_collection:trav_fun()) -> any().
-spec has       (any()) -> boolean().
-spec internals () -> b_collection:internals().
-spec is_empty  () -> boolean(). 
-spec map       (b_collection:trav_fun()) -> b_collection:collection().
-spec merge     (b_collection:merge_fun(), b_collection:collection()) -> b_collection:collection().
-spec put       (any()) -> b_collection:collection().
-spec to_erlang () -> any().

-endif.

-endif.