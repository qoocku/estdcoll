%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc Erlang Standard Collection List Implementation.
%%% @end
%%% ==========================================================================
-module(i_map_dict, [Dict]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
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
  instance(dict:new()).

new (List) when is_list(List) ->
  new(dict:from_list(List));
new (D) ->
  instance(D).

all (Pred) when is_function(Pred) ->
  fold(fun (Item, Bool) -> Bool andalso Pred(Item) end, true).  

any (Pred) when is_function(Pred) ->
  lists:any(Pred, dict:to_list(Dict)).

at (Key) ->
  case dict:find(Key, Dict) of
    {ok, Value} -> Value;
    error       -> exit(badarg)
  end.

delete (Item = {Key, _Value}) ->
  case has(Item) of
    true  -> new(dict:erase(Key, Dict));
    false -> THIS
  end;
delete (Key) ->
  new(dict:erase(Key, Dict)).

extend (Dict2) ->
  merge(fun (_, _) -> exit(badarg) end, Dict2).

fetch (Key) ->
  case dict:find(Key, Dict) of
    none -> undefined;
    {ok, V} -> {ok, V}
  end.

filter (Pred) when is_function(Pred) ->
  new(dict:filter(fun (Key, Value) -> Pred({Key, Value}) end, Dict)).

foreach (Fun) when is_function(Fun) ->
  lists:foreach(Fun, dict:to_list(Dict)).

fold (Fun, Acc) ->
  dict:fold(fun (Key, Value, Acc0) ->
                Fun({Key, Value}, Acc0)
            end, Acc, Dict).

has ({Key, Value}) ->
  case dict:find(Key, Dict) of
    {ok, Value}  -> true;
    {ok, _Other} -> false;
    error        -> false
  end;
has (Key) ->
  dict:is_key(Key, Dict).

is_empty () ->
  dict:size(Dict) == 0.

internals () ->
  Dict.

map (Fun) when is_function(Fun) ->
  new(lists:map(Fun, dict:to_list(Dict))).

map_values (Fun) when is_function(Fun) ->
  new(dict:map(Fun, Dict)).

merge (Fun, Dict2) when is_function(Fun),
                         element(1, Dict2) =:= ?MODULE ->
  new(dict:merge(fun (K, V, V2) -> Fun({K, V}, V2) end, Dict, Dict2:internals()));
merge (Fun, Dict2) when is_function(Fun) ->
  try dict:size(Dict2) of
    0 -> THIS;
    _ -> new(dict:merge(fun (K, V, V2) -> Fun({K, V}, V2) end, Dict, Dict2)) 
  catch
    _:R -> exit({badarg, R})
  end.

put ({Key, Value}) ->
  new(dict:store(Key, Value, Dict)).

put (Key, Value) ->
  put({Key, Value}).

size () ->
  dict:size(Dict).

to_erlang () ->
  Dict.

to_map () ->
  THIS.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================
