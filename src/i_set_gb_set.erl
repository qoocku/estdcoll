%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-18
%%% @doc Erlang Standard Set implemented as gb_set.
%%% @end
%%% ==========================================================================
-module  (i_set_gb_set, [Type, Set]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

-behavior (b_set).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/0, new/1]).

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
  new(gb_sets:empty()).

new (List) when is_list(List) ->
  new(gb_sets:from_list(List));
new ({ordset, List}) when is_list(List) ->
  new(gb_sets:from_ordset(List));
new (S) ->
  case gb_sets:is_set(S) of
    true  -> instance(set, S);
    false -> exit(badarg)
  end.

all (Pred) when is_function(Pred) ->
  fold(fun (Item, Bool) -> Bool andalso Pred(Item) end, true).  

any (Pred) when is_function(Pred) ->
  lists:any(Pred, gb_sets:to_list(Set)).

delete (Item) ->
  new(gb_sets:delete_any(Item, Set)).
      
extend (Set2) when is_tuple(Set2) andalso element(1, Set2) =:= ?MODULE ->
  extend(Set2:to_erlang());
extend (Set2) when is_tuple(Set2) ->
  {Src, Tgt} = case gb_sets:size(Set2) > size() of
                 true  -> {Set, Set2};
                 false -> {Set2, Set}
               end,
  new(extend_loop(gb_sets:next(gb_sets:iterator(Src)), Tgt)).

filter (Pred) when is_function(Pred) ->
  Iter = gb_sets:iterator(Set),
  new(filter_loop(Pred, gb_sets:next(Iter), [])).

foreach (Fun) when is_function(Fun) ->
  foreach_loop(Fun, gb_sets:next(gb_sets:iterator(Set))).

fold (Fun, Acc) when is_function(Fun) ->
  fold_loop(Fun, gb_sets:next(gb_sets:iterator(Set)), Acc).

has (Item) ->
  gb_sets:is_member(Item, Set).

is_empty () ->
  gb_sets:is_empty(Set).

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
  gb_sets:is_disjoint(Set, Set1);
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
  new(gb_sets:intersection(Set, Set1));
intersection (Iter) when is_tuple(Iter) andalso
                         element(2, Iter) =:= iterator ->
  intersection_loop(Iter, gb_sets:empty());
intersection (Collection) when is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> intersection_loop(Iter, gb_sets:empty())
  catch
    _:_ ->
       exit(badarg)
  end.

iterator () ->
  i_iterator_gb_set:new(gb_sets:iterator(Set)).

map (Fun) when is_function(Fun) ->
  new(map_loop(Fun, gb_sets:next(gb_sets:iterator(Set)), [])).

union (Set2) when is_tuple(Set2) andalso
                  element(1, Set2) =:= ?MODULE ->
  new(gb_sets:union(Set, Set2:to_erlang()));
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
  merge(Fun, {gb_set, Set2:to_erlang()});
merge (Fun, Iter) when is_function(Fun) andalso
                         is_tuple(Iter) andalso
                         element(2, Iter) =:= iterator ->
  new(merge_iterator_loop(Fun, Iter, Set));
merge (Fun, Collection) when is_function(Fun) andalso 
                             is_tuple(Collection) ->
  try Collection:iterator() of
    Iter -> new(merge_iterator_loop(Fun, Iter, Set))
  catch
    _:_ -> merge(Fun, {gb_set, Collection})
  end;
merge (Fun, {gb_set, Set2}) when is_function(Fun) ->
  {Src, Tgt} = case size() > gb_sets:size(Set2) of
                 true  -> {Set2, Set};
                 false -> {Set, Set2}
               end,
  new(merge_loop(Fun, gb_sets:next(gb_sets:iterator(Src)), Tgt)).


put (Item) ->
  new(gb_sets:add(Item, Set)).

size () ->
  gb_sets:size(Set).

subtract (Set1) when is_tuple(Set) andalso
                     element(1, Set) =:= ?MODULE ->
  new(gb_sets:subtract(Set, Set1));
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

filter_loop (_, none, NewSet) ->
  NewSet;
filter_loop (Pred, {Item, Iter}, Set1) ->
  Set2 = case Pred(Item) of
            true  -> [Item | Set1]; 
            false -> Set1
          end,
  filter_loop(Pred, gb_sets:next(Iter), Set2).

foreach_loop (_, none) ->
  ok;
foreach_loop (Fun, {Item, Iter}) ->
  Fun(Item),
  foreach_loop(Fun, gb_sets:next(Iter)).

fold_loop (_, none, Acc) ->
  Acc;
fold_loop (Fun, {Item, Iter}, Acc) ->
  fold_loop(Fun, gb_sets:next(Iter), Fun(Item, Acc)).

map_loop (_, none, NewSet) ->
  NewSet;
map_loop (Fun, {Item, Iter}, Set1) ->
  map_loop(Fun, gb_sets:next(Iter), [Fun(Item) | Set1]).

merge_iterator_loop (Fun, Iter, Set1) ->
  try Iter:next() of
    {Item, Next} ->   case gb_sets:is_member(Item, Set1) of
                        true  ->
                          merge_iterator_loop(Fun, Next, gb_sets:add(Fun(Item), Set1));
                        false ->
                          merge_iterator_loop(Fun, Next, gb_sets:add(Item, Set1))
                      end
  catch
    exit:bad_iterator ->
      Set1
  end.

merge_iterator_loop (Iter, Set1) ->
  try Iter:next() of
    {Item, Next} -> merge_iterator_loop(Next, gb_sets:add(Item, Set1))
  catch
    exit:bad_iterator ->
      Set1
  end.

merge_loop (_, none, Set1) ->
  Set1;
merge_loop (Fun, {Item, Next}, Set1) ->
  case gb_sets:is_member(Item, Set1) of
    true  -> merge_loop(Fun, Next, gb_sets:add(Fun(Item), Set1));
    false -> merge_loop(Fun, Next, gb_sets:add(Item, Set1))
  end;
merge_loop (Fun, Iter, Set1) ->
  merge_loop(Fun, gb_sets:next(Iter), Set1).
      
extend_loop (none, NewSet) ->
  NewSet;
extend_loop ({Item, Iter}, Set1) ->
  NewSet = try
              gb_sets:insert(Item, Set1)
            catch
              _:_ -> exit(badarg)
            end,
  extend_loop(gb_sets:next(Iter), NewSet).

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
      

  
