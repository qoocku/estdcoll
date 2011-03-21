%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc Erlang Standard Collection List Implementation.
%%% @end
%%% ==========================================================================
-module(i_collection_list, [Type, List]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

-behavior (b_ordered_collection).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/0, new/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (COLLECTION_BEHAVIOR_SPECS, true).
-define (COLLECTION_BEHAVIOR_EXPORTS, true).
-define (ORDERED_COLLECTION_BEHAVIOR_SPECS, true).
-define (ORDERED_COLLECTION_BEHAVIOR_EXPORTS, true).
-define (RANDOM_ACCESS_COLLECTION_BEHAVIOR_EXPORTS, true).
-define (STRICT_COLLECTION_BEHAVIOR_EXPORTS, true).
-include_lib ("estdcoll/include/strict_collection.hrl").
-include_lib ("estdcoll/include/random_access_collection.hrl").
-include_lib ("estdcoll/include/ordered_collection.hrl").

-compile([{inline, [{all,    1},
                    {any,    1},
                    {at,     1},
                    {delete, 1},
                    {extend, 1},
                    {filter, 1},
                    {get, 1},
                    {fold, 2},
                    {foldl, 2},
                    {foldr, 2},
                    {foreach, 1},
                    {has, 1},
                    {is_empty, 0},
                    {internals, 0},
                    {iterator, 0},
                    {map, 1},
                    {merge, 2},
                    {put,   1},
                    {reverse, 0},
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
  new([]).

new (Xs) when is_list(Xs) ->
  instance(list, Xs).

%%% ============================================================================
%%% B e h a v i o r  F u n c t i o n s
%%% ============================================================================

any (Pred) when is_function(Pred) ->
  lists:any(Pred, List).

all (Pred) when is_function(Pred) ->
  lists:all(Pred, List).

at (Index) when Index >= 0 andalso Index =< length(List) ->
  lists:nth(Index, List);
at (_Index) ->
  exit(badarg).

append (List2) when is_list(List2) ->
  new(List ++ List2);
append (List2) when is_tuple(List2),
                     element(1, List2) =:= ?MODULE ->
  new(List ++ List2:internals());
append (Item) ->
  append([Item]).

delete (Item) ->
  new(lists:delete(Item, List)).

extend (List2) ->
  append(List2).

filter (Pred) ->
  new(lists:filter(Pred, List)).

fold (Fun, Acc) ->
  foldl(Fun, Acc).

foldr (Fun, Acc) when is_function(Fun) ->
  lists:foldr(Fun, Acc, List).

foldl (Fun, Acc) when is_function(Fun) ->
  lists:foldl(Fun, Acc, List).

foreach (Fun) when is_function(Fun) ->
  lists:foreach(Fun, List).

get (Index) when Index > 0 andalso Index =< length(List) ->
  {ok, at(Index)};
get (_Index) ->
  undefined.
  
has (Item) ->
  lists:member(Item, List).

internals () ->
  List.

is_empty () ->
  List =:= [].

iterator () ->
  i_iterator_list:new(List).

map (Fun) when is_function(Fun) ->
  new([Fun(I) || I <- List]).

merge (Fun, List2) when is_function(Fun),
                         is_list(List2) ->
  new(lists:merge(Fun, List, List2));
merge (Fun, List2) when is_function(Fun) ->
  new(lists:merge(Fun, List, List2:internals())).

prepend (List2) when is_tuple(List2),
                      element(1, List2) =:= ?MODULE ->
  List2:append(List);
prepend (List2) when is_list(List2) ->
  new(List2 ++ List);
prepend (Item) ->
  new([Item | List]).

put (Item) ->
  case has(Item) of
    true  -> THIS;
    false -> prepend(Item)
  end.

reverse () ->
  new(lists:reverse(List)).

size () ->
  length(List).

to_erlang () ->
  List.


%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

