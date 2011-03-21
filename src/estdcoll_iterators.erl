-module (estdcoll_iterators).
-author ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

-export ([do_next/3,
          fold_loop/5,
          foreach_loop/4,
          iterator_to_list/1]).

do_next ({Mod, Fun}, Oper, I) ->
  case apply(Mod, Fun, [I]) of
    {K, V, N} -> {Oper({K, V}), N};
    none      -> none
  end.

fold_loop (_, _, _, none, Acc) ->
  Acc;
fold_loop (Next, Fun, Oper, {Item, I}, Acc) ->
  fold_loop(Next, Fun, Oper, do_next(Next, Oper, I), Fun(Item, Acc));
fold_loop (Next, Fun, Oper, I, Acc) ->
  fold_loop(Next, Fun, Oper, do_next(Next, Oper, I), Acc).

foreach_loop (_, _, _, none) ->
  ok;
foreach_loop (Next, Fun, Oper, {Item, I}) ->
  Fun(Item),
  foreach_loop(Next, Fun, Oper, do_next(Next, Oper, I));
foreach_loop (Next, Fun, Oper, Iter) ->
  foreach_loop(Next, Fun, Oper, do_next(Next, Oper, Iter)).

iterator_to_list (Iter) ->
  iterator_to_list(Iter, []).

iterator_to_list (Iter, Acc) ->
  try Iter:next() of
    {Item, Next} ->
      iterator_to_list(Next, [Item | Acc])
  catch
    exit:bad_iterator ->
      lists:reverse(Acc)
  end.

      
