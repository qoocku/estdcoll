-module (estdcoll_iterators).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

-export ([all_loop/4,
          any_loop/4,
          do_next/3,
          fold_loop/5,
          foreach_loop/4,
          iterator_to_list/1]).

-include ("estdcoll/include/iterator.hrl").

-opaque repr(T) :: {repr, T}.
-opaque iterator() :: module().

-export_types ([iterator/0,
                repr/1]).

-spec do_next ({module(), atom()},
               b_collection:trav_fun(),
               repr(any())) -> {any(), iterator()} | none.
                  

do_next ({Mod, Fun}, Oper, #repr{r = I}) ->
  case apply(Mod, Fun, [I]) of
    {Item, N} -> {Oper(Item), N};
    none -> none
  end.

fold_loop (_, _, _, none, Acc) ->
  Acc;
fold_loop (_, Fun, _, {Item, none}, Acc) ->
  Fun(Item, Acc);
fold_loop (Next, Fun, Oper, R = #repr{}, Acc) ->
  fold_loop(Next, Fun, Oper, do_next(Next, Oper, R), Acc);
fold_loop (Next, Fun, Oper, {Item, I}, Acc) ->
  fold_loop(Next, Fun, Oper, do_next(Next, Oper, #repr{r = I}), Fun(Item, Acc)).

foreach_loop (_, _, _, none) ->
  ok;
foreach_loop (_, Fun, _, {Item, none}) ->
  Fun(Item),
  ok;  
foreach_loop (Next, Fun, Oper, R = #repr{}) ->
  foreach_loop(Next, Fun, Oper, do_next(Next, Oper, R));
foreach_loop (Next, Fun, Oper, {Item, I}) ->
  Fun(Item),
  foreach_loop(Next, Fun, Oper, do_next(Next, Oper, #repr{r = I})).

iterator_to_list (Iter) ->
  iterator_to_list(Iter, []).

iterator_to_list (none, Acc) ->
  lists:reverse(Acc);
iterator_to_list (Iter, Acc) ->
  try Iter:next() of      
    {Item, Next} ->
      iterator_to_list(Next, [Item | Acc])
  catch
    exit:bad_iterator ->
      lists:reverse(Acc)
  end.

all_loop (_, Fun, _, {Item, none}) ->
  Fun(Item);
all_loop (Next, Fun, Oper, R = #repr{}) ->
  all_loop(Next, Fun, Oper, do_next(Next, Oper, R));
all_loop (Next, Fun, Oper, {Item, I}) ->
  case Fun(Item) of
    true   -> all_loop(Next, Fun, Oper, do_next(Next, Oper, #repr{r = I}));
    false  -> false
  end.
      
any_loop (_, Fun, _, {Item, none}) ->
  Fun(Item);
any_loop (Next, Fun, Oper, R = #repr{}) ->
  any_loop(Next, Fun, Oper, do_next(Next, Oper, R));
any_loop (Next, Fun, Oper, {Item, I}) ->
  case Fun(Item) of
    false -> any_loop(Next, Fun, Oper, do_next(Next, Oper, #repr{r = I}));
    true  -> true
  end.
