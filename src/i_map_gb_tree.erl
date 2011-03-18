%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-18
%%% @doc Erlang Standard Map implemented as gb_tree
%%% @end
%%% ==========================================================================
-module  (i_map_gb_tree, [Tree]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

-behavior (b_map).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/0, new/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (COLLECTION_BEHAVIOR_SPECS, true).
-define (COLLECTION_BEHAVIOR_EXPORTS, true).
-define (RANDOM_ACCESS_COLLECTION_BEHAVIOR_SPECS, true).
-define (RANDOM_ACCESS_COLLECTION_BEHAVIOR_EXPORTS, true).
-define (STRICT_COLLECTION_BEHAVIOR_SPECS, true).
-define (STRICT_COLLECTION_BEHAVIOR_EXPORTS, true).
-define (MAP_BEHAVIOR_SPECS, true).
-define (MAP_BEHAVIOR_EXPORTS, true).
-include_lib ("estdcoll/include/strict_collection.hrl").
-include_lib ("estdcoll/include/random_access_collection.hrl").
-include_lib ("estdcoll/include/map.hrl").

-compile([{inline, [{all,    1},
                    {any,    1},
                    {at,     1},
                    {delete, 1},
                    {extend, 1},
                    {filter, 1},
                    {fold, 2},
                    {foreach, 1},
                    {has, 1},
                    {is_empty, 0},
                    {internals, 0},
                    {map, 1},
                    {merge, 2},
                    {put,   1},
                    {put, 2},
                    {size, 0},
                    {to_erlang, 0}]}]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

new () ->
  instance(gb_trees:empty()).

new (List) when is_list(List) ->
  new(gb_trees:from_orddict(List));
new (D) ->
  instance(D).

all (Pred) when is_function(Pred) ->
  fold(fun (Item, Bool) -> Bool andalso Pred(Item) end, true).  

any (Pred) when is_function(Pred) ->
  lists:any(Pred, gb_trees:to_list(Tree)).

at (Key) ->
  gb_trees:get(Key, Tree).

delete (Item = {Key, _Value}) ->
  delete(Key);
delete (Key) ->
  new(gb_trees:delete_any(Key, Tree)).
      
extend (Tree2) ->
  merge(fun (_, _) -> exit(badarg) end, Tree2).

fetch (Key) ->
  case gb_trees:lookup(Key, Tree) of
    none -> undefined;
    {value, V} -> {ok, V}
  end.

filter (Pred) when is_function(Pred) ->
  Iter = gb_trees:iterator(Tree),
  new(filter_loop(Pred, gb_trees:next(Iter), [])).

foreach (Fun) when is_function(Fun) ->
  foreach_loop(Fun, gb_trees:next(gb_trees:iterator(Tree))).

fold (Fun, Acc) when is_function(Fun) ->
  fold_loop(Fun, gb_trees:next(gb_trees:iterator(Tree)), Acc).

has ({Key, Value}) ->
  case fetch(Key) of
    {ok, Value}  -> true;
    {ok, _Other} -> false;
    undefined    -> false
  end;
has (Key) ->
  gb_trees:is_defined(Key, Tree).

is_empty () ->
  gb_trees:is_empty(Tree).

internals () ->
  Tree.

map (Fun) when is_function(Fun) ->
  new(map_loop(Fun, gb_trees:next(gb_trees:iterator(Tree)), [])).

map_values (Fun) when is_function(Fun) ->
  new(gb_trees:map(fun (K, V) -> Fun(K, V) end, Tree)).

merge (Fun, Tree2) when is_function(Fun),
                         element(1, Tree2) =:= ?MODULE ->
  exit(not_implemented);
merge (Fun, Tree2) when is_function(Fun) ->
  exit(not_implemented).

put ({Key, Value}) ->
  case fetch(Key) of
    {ok, Value} -> THIS;
    {ok, _}     -> new(gb_trees:update(Key, Value, Tree));
    undefined   -> new(gb_trees:insert(Key, Value, Tree))
  end.

put (Key, Value) ->
  put({Key, Value}).

size () ->
  gb_trees:size(Tree).

to_erlang () ->
  Tree.

to_map () ->
  THIS.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

filter_loop (_, none, NewTree) ->
  NewTree;
filter_loop (Pred, {Key, Val, Iter}, Tree1) ->
  Tree2 = case Pred(Item = {Key, Val}) of
            true  -> [Item | Tree1]; 
            false -> Tree1
          end,
  filter_loop(Pred, gb_trees:next(Iter), Tree2).

foreach_loop (_, none) ->
  ok;
foreach_loop (Fun, {Key, Val, Iter}) ->
  Fun({Key, Val}),
  foreach_loop(Fun, gb_trees:next(Iter)).

fold_loop (_, none, Acc) ->
  Acc;
fold_loop (Fun, {Key, Val, Iter}, Acc) ->
  fold_loop(Fun, gb_trees:next(Iter), Fun({Key, Val}, Acc)).

map_loop (_, none, NewTree) ->
  NewTree;
map_loop (Fun, {Key, Val, Iter}, Tree1) ->
  map_loop(Fun, gb_trees:next(Iter), [Fun({Key, Val}) | Tree1]).
