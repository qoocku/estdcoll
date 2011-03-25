%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-18
%%% @doc Erlang Standard Set implemented as ordinary set.
%%% @end
%%% ==========================================================================
-module  (i_set_set, [Type, Set, Mod]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

-behavior (b_set).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/0, new/1, new/2]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (COLLECTION_BEHAVIOR_SPECS, true).
-define (COLLECTION_BEHAVIOR_EXPORTS, true).
-define (STRICT_COLLECTION_BEHAVIOR_SPECS, true).
-define (STRICT_COLLECTION_BEHAVIOR_EXPORTS, true).
-define (SET_BEHAVIOR_SPECS, true).
-define (SET_BEHAVIOR_EXPORTS, true).
-include_lib ("estdcoll/include/strict_collection.hrl").
-include_lib ("estdcoll/include/random_access_collection.hrl").
-include_lib ("estdcoll/include/set.hrl").

-compile([{inline, [{all,    1},
                    {any,    1},
                    {delete, 1},
                    {extend, 1},
                    {filter, 1},
                    {fold, 2},
                    {foreach, 1},
                    {has, 1},
                    {is_empty, 0},
                    {internals, 0},
                    {iterator, 0},
                    {map, 1},
                    {merge, 2},
                    {put,   1},
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
  new(sets, sets:new()).

new (Mod) when Mod =:= sets orelse Mod =:= ordsets ->
  new (Mod, Mod:new());
new ({Mod, S}) when is_list(S) andalso
                    (Mod =:= sets orelse Mod =:= ordsets) ->
  new(Mod, Mod:from_list(S));
new ({sets, S}) ->
  new(sets, S);
new (S) ->
  new({sets, S}).

new (Mod, S) ->
  case Mod:is_set(S) of
    true  -> instance(set, S, Mod);
    false -> exit(badarg)
  end.

all (Pred) when is_function(Pred) ->
  fold(fun (Item, Bool) -> Bool andalso Pred(Item) end, true).  

any (Pred) when is_function(Pred) ->
  lists:any(Pred, Mod:to_list(Set)).

delete (Item) ->
  new(Mod, Mod:del_element(Item, Set)).
      
extend (Set2) ->
  union(Set2).

filter (Pred) when is_function(Pred) ->
  new(Mod, Mod:filter(Pred, Set)).

foreach (Fun) when is_function(Fun) ->
  lists:foreach(Fun, Mod:to_list(Set)).

fold (Fun, Acc) when is_function(Fun) ->
  Mod:fold(Fun, Acc, Set).

has (Item) ->
  Mod:is_element(Item, Set).

is_empty () ->
  Mod:size(Set) == 0.

internals () ->
  Set.

isa (set) ->
  true;
isa (collection) ->
  true;
isa (_) ->
  false.

is_disjoint (Set1) when is_tuple(Set) andalso
                        element(1, Set) =:= ?MODULE ->
  Mod:is_disjoint(Set, Set1);
is_disjoint (Iter) when is_tuple(Iter) andalso
                        element(2, Iter) =:= iterator ->
  is_disjoint_loop(Iter);
is_disjoint (Collection) when is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> is_disjoint_loop(Iter)
  catch
    _:_ ->
       exit(badarg)
  end.

intersection (Set1) when is_tuple(Set) andalso
                         element(1, Set) =:= ?MODULE ->
  new(sets:intersection(Set, Set1));
intersection (Iter) when is_tuple(Iter) andalso
                         element(2, Iter) =:= iterator ->
  intersection_loop(Iter, Mod:new());
intersection (Collection) when is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> intersection_loop(Iter, Mod:new())
  catch
    _:_ ->
       exit(badarg)
  end.

iterator () ->
  i_iterator_set:new({Mod, Set}).

map (Fun) when is_function(Fun) ->
  new({Mod, [Fun(Item) || Item <- Mod:to_list(Set)]}).

union (Set2) when is_tuple(Set2) andalso
                  element(1, Set2) =:= ?MODULE ->
  new(Mod, Mod:union(Set, Set2:to_erlang()));
union (Iter) when is_tuple(Iter) andalso
                  element(2, Iter) =:= iterator ->
  new(merge_iterator_loop(Iter, Set));
union (Collection) when is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> new(merge_iterator_loop(Iter, Set))
  catch
    _:_ -> exit(badarg)
  end.

merge (Fun, Collection) when is_function(Fun) andalso 
                             is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> new(merge_iterator_loop(Iter, Set))
  catch
    _:_ -> exit(badarg)
  end;
merge (Fun, Set2) when is_function(Fun) andalso
                       is_tuple(Set2) andalso
                       element(1, Set2) =:= ?MODULE ->
  merge(Fun, {set, Set2:to_erlang()});
merge (Fun, Iter) when is_function(Fun) andalso
                         is_tuple(Iter) andalso
                         element(2, Iter) =:= iterator ->
  new(merge_iterator_loop(Fun, Iter, Set));
merge (Fun, Collection) when is_function(Fun) andalso 
                             is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> new(merge_iterator_loop(Fun, Iter, Set))
  catch
    _:_ -> merge(Fun, {set, Collection})
  end;
merge (Fun, {set, Set2}) when is_function(Fun) ->
  {Src, Tgt} = case size() > Mod:size(Set2) of
                 true  -> {Set2, Set};
                 false -> {Set, Set2}
               end,
  new(merge_loop(Fun, Mod:to_list(Src), Tgt)).


put (Item) ->
  new(Mod, Mod:add_element(Item, Set)).

size () ->
  Mod:size(Set).

subtract (Set1) when is_tuple(Set) andalso
                     element(1, Set) =:= ?MODULE ->
  new(Mod, Mod:subtract(Set, Set1));
subtract (Iter) when is_tuple(Iter) andalso
                     element(2, Iter) =:= iterator ->
  subtract_loop(Iter, Set);
subtract (Collection) when is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> subtract_loop(Iter, Set)
  catch
    _:_ ->
       exit(badarg)
  end.


to_erlang () ->
  Set.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

merge_iterator_loop (Fun, Iter, Set1) ->
  try Iter:next() of
    {Item, Next} ->   NewItem = case Mod:is_member(Item, Set1) of
                                  true  -> Fun(Item);
                                  false -> Item
                                end,
                      merge_iterator_loop(Fun, Next, Mod:add_element(NewItem, Set1))
  catch
    exit:bad_iterator ->
      Set1
  end.

merge_iterator_loop (Iter, Set1) ->
  try Iter:next() of
    {Item, Next} -> merge_iterator_loop(Next, Mod:add_element(Item, Set1))
  catch
    exit:bad_iterator ->
      Set1
  end.

merge_loop (_, none, Set1) ->
  Set1;
merge_loop (Fun, {Item, Next}, Set1) ->
 NewItem =  case Mod:is_member(Item, Set1) of
              true  -> Fun(Item);
              false -> Item
            end,
  merge_loop(Fun, Next, Mod:add_element(NewItem, Set1));
merge_loop (Fun, Iter, Set1) ->
  merge_loop(Fun, Iter:next(), Set1).
      
is_disjoint_loop (Iter) ->
  try Iter:next() of
    {Val, Next} ->
      case has(Val) of
        true  -> false;
        false -> is_disjoint_loop(Next)
      end
  catch
    _:_ -> false
  end.

any_loop (none, _, _, NewSet) ->
  NewSet;
any_loop ({Val, Next}, Mod, Fun, NewSet) ->
  Set2 = case has(Val) of
           true  -> Mod:Fun(Val, NewSet);
           false -> NewSet
         end,
  any_loop(Next:next(), Mod, Fun, Set2);
any_loop (Iter, Mod, Fun, NewSet) ->
  any_loop(Iter:next(), Mod, Fun, NewSet).

intersection_loop (Iter, NewSet) ->
  any_loop(Iter, gb_sets, add_element, NewSet).

subtract_loop (Iter, NewSet) ->
  any_loop(Iter, gb_sets, del_element, NewSet).
      

  
