%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc Erlang Standard Map Implementation as `dict' or `orddict'.
%%% @end
%%% ==========================================================================
-module(i_map_dict, [Type, Dict, Mod]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

-behavior (b_map).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/0, new/1, new/2]).

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
                    {get, 1},
                    {has, 1},
                    {is_empty, 0},
                    {internals, 0},
                    {iterator, 0},
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
  new(dict).

new (Mod) when Mod =:= dict orelse Mod =:= orddict ->
  new(Mod, Mod:new());
new (List) when is_list(List) ->
  new({dict, List});
new ({Mod, List}) when (Mod =:= dict orelse Mod =:= orddict) andalso
                       is_list(List) ->
  new(Mod, Mod:from_list(List)).

new (Mod, D) when Mod =:= dict orelse Mod =:= orddict  ->
  instance(map, D, Mod).

all (Pred) when is_function(Pred) ->
  fold(fun (Item, Bool) -> Bool andalso Pred(Item) end, true).  

any (Pred) when is_function(Pred) ->
  lists:any(Pred, Mod:to_list(Dict)).

at (Key) ->
  case Mod:find(Key, Dict) of
    {ok, Value} -> Value;
    error       -> exit(badarg)
  end.

delete (Item = {Key, _Value}) ->
  case has(Item) of
    true  -> new(Mod, Mod:erase(Key, Dict));
    false -> THIS
  end;
delete (Key) ->
  new(Mod, Mod:erase(Key, Dict)).

extend (Dict2) ->
  merge(fun (_, _) -> exit(badarg) end, Dict2).

get (Key) ->
  case Mod:find(Key, Dict) of
    none -> undefined;
    {ok, V} -> {ok, V}
  end.

filter (Pred) when is_function(Pred) ->
  new(Mod, Mod:filter(fun (Key, Value) -> Pred({Key, Value}) end, Dict)).

foreach (Fun) when is_function(Fun) ->
  lists:foreach(Fun, Mod:to_list(Dict)).

fold (Fun, Acc) ->
  Mod:fold(fun (Key, Value, Acc0) ->
                Fun({Key, Value}, Acc0)
            end, Acc, Dict).

has ({Key, Value}) ->
  case Mod:find(Key, Dict) of
    {ok, Value}  -> true;
    {ok, _Other} -> false;
    error        -> false
  end;
has (Key) ->
  Mod:is_key(Key, Dict).

isa (collection) ->
  true;
isa (map) ->
  true;
isa (_) ->
  false.

is_empty () ->
  Mod:size(Dict) == 0.

internals () ->
  Dict.

iterator () ->
  i_iterator_dict:new({Mod, Dict}).

map (Fun) when is_function(Fun) ->
  new({Mod, lists:map(Fun, Mod:to_list(Dict))}).

map_values (Fun) when is_function(Fun) ->
  new(Mod, Mod:map(fun (K, V) -> Fun({K, V}) end, Dict)).

merge (Fun, Dict2) when is_function(Fun) andalso
                        is_tuple(Dict2) andalso
                        element(1, Dict2) =:= ?MODULE ->
  new(Mod, Mod:merge(fun (K, V, V2) -> Fun({K, V}, V2) end, Dict, Dict2:internals()));
merge (Fun, Dict2) when is_function(Fun) andalso
                        is_tuple(Dict2) andalso 
                        element(1, Dict2) =:= dict ->
  try Mod:size(Dict2) of
    0 -> THIS;
    _ -> new(Mod, Mod:merge(fun (K, V, V2) -> Fun({K, V}, V2) end, Dict, Dict2)) 
  catch
    _:R -> exit({badarg, R})
  end;
merge (Fun, Iter) when is_function(Fun) andalso
                       is_tuple(Iter) andalso 
                       element(2, Iter) =:= iterator ->
  new(Mod, Mod:merge(fun (K, V, V2) -> Fun({K, V}, V2) end,
                     Dict, 
                     Mod:from_list(estdcoll_iterators:iterator_to_list(Iter))));
merge (Fun, Collection) when is_function(Fun) andalso
                       is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> merge(Fun, Iter)
  catch
    _:_ -> exit(badarg)
  end.

put ({Key, Value}) ->
  new(Mod, Mod:store(Key, Value, Dict)).

put (Key, Value) ->
  put({Key, Value}).

size () ->
  Mod:size(Dict).

to_erlang () ->
  Dict.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================
