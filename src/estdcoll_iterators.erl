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

-type next_result() :: #nxt{} | none.

-opaque repr(T) :: {repr, T}.
-opaque iterator() :: module().

-export_types ([iterator/0,
                repr/1]).

-spec do_next ({module(), atom()},
               b_collection:trav_fun(),
               repr(any())) -> next_result().
                  

do_next ({Mod, Fun}, Oper, #repr{r = I}) ->
  case apply(Mod, Fun, [I]) of
    {{Item, N}, {oper, NewOper}, {next, NewNext}} ->
      #nxt{pair = {NewOper(Item), N}, 
           oper = NewOper,
           next = NewNext};
    {{Item, N}, {next, NewNext}} ->
      #nxt{pair = {Oper(Item), N}, 
           oper = Oper,
           next = NewNext};
    {{Item, N}, {oper, NewOper}} ->
      #nxt{pair = {NewOper(Item), N}, 
           oper = NewOper,
           next = {Mod, Fun}};
    {Item, N} -> 
      #nxt{pair = {Oper(Item), N},
           oper = Oper,
           next = {Mod, Fun}};
    none -> 
      none
  end.

fold_loop (Next, Fun, Oper, R = #repr{}, Acc) ->
  fold_loop(Fun, do_next(Next, Oper, R), Acc).

fold_loop (_, none, Acc) ->
  Acc;
fold_loop (Fun, #nxt{pair = {Item, none}}, Acc) ->
  Fun(Item, Acc);
fold_loop (Fun, #nxt{pair = {Item, I}, 
                     oper = NewOper,
                     next = NewNext}, Acc) ->
  fold_loop(Fun, do_next(NewNext, NewOper, #repr{r = I}), Fun(Item, Acc)).

foreach_loop (Next, Fun, Oper, R = #repr{}) ->
  foreach_loop(Fun, do_next(Next, Oper, R)).

foreach_loop (_, none) ->
  ok;
foreach_loop (Fun, #nxt{pair = {Item, none}}) ->
  Fun(Item),
  ok;  
foreach_loop (Fun, #nxt{pair = {Item, I},
                        oper = NewOper,
                        next = NewNext}) ->
  Fun(Item),
  foreach_loop(Fun, do_next(NewNext, NewOper, #repr{r = I})).

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

all_loop (Next, Fun, Oper, R = #repr{}) ->
  all_loop(Fun, do_next(Next, Oper, R)).

all_loop (Fun, #nxt{pair = {Item, none}}) ->
  Fun(Item);
all_loop (Fun, #nxt{pair = {Item, I},
                    oper = NewOper,
                    next = NewNext}) ->
  case Fun(Item) of
    true   -> all_loop(Fun, do_next(NewNext, NewOper, #repr{r = I}));
    false  -> false
  end.

any_loop (Next, Fun, Oper, R = #repr{}) ->
  any_loop(Fun, do_next(Next, Oper, R)).
      
any_loop (Fun, #nxt{pair = {Item, none}}) ->
  Fun(Item);
any_loop (Fun, #nxt{pair = {Item, I},
                    oper = NewOper,
                    next = NewNext}) ->
  case Fun(Item) of
    false -> any_loop(Fun, do_next(NewNext, NewOper, #repr{r = I}));
    true  -> true
  end.
