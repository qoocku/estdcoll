-module (estdcoll_iterators).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

-export ([all_loop/4,
          any_loop/4,
          do_next/3,
          fold_loop/5,
          foreach_loop/4,
          iterator_to_list/1]).

do_next ({Mod, Fun}, Oper, I) ->
  case apply(Mod, Fun, [I]) of
    {Item, N} -> {Oper(Item), N};
    none -> none
  end.

fold_loop (_, Fun, _, {Item, none}, Acc) ->
  Fun(Item, Acc);
fold_loop (Next, Fun, Oper, {Item, I}, Acc) ->
  fold_loop(Next, Fun, Oper, do_next(Next, Oper, I), Fun(Item, Acc));
fold_loop (Next, Fun, Oper, I, Acc) ->
  fold_loop(Next, Fun, Oper, do_next(Next, Oper, I), Acc).

foreach_loop (_, Fun, _, {Item, none}) ->
  Fun(Item),
  ok;  
foreach_loop (Next, Fun, Oper, {Item, I}) ->
  Fun(Item),
  foreach_loop(Next, Fun, Oper, do_next(Next, Oper, I));
foreach_loop (Next, Fun, Oper, Iter) ->
  foreach_loop(Next, Fun, Oper, do_next(Next, Oper, Iter)).

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
all_loop (Next, Fun, Oper, {Item, I}) ->
  case Fun(Item) of
    true   -> all_loop(Next, Fun, Oper, do_next(Next, Oper, I));
    false  -> false
  end;
all_loop (Next, Fun, Oper, I) ->
  all_loop(Next, Fun, Oper, do_next(Next, Oper, I)).
      
any_loop (_, Fun, _, {Item, none}) ->
  Fun(Item);
any_loop (Next, Fun, Oper, {Item, I}) ->
  case Fun(Item) of
    false -> any_loop(Next, Fun, Oper, do_next(Next, Oper, I));
    true  -> true
  end;
any_loop (Next, Fun, Oper, I) ->
  any_loop(Next, Fun, Oper, do_next(Next, Oper, I)).
