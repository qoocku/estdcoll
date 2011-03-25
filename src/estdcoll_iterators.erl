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
  try do_next(Next, Oper, R) of
      N = #nxt{} -> fold_loop(Fun, N, Acc)
  catch
    exit:bad_iterator -> Acc
  end.

fold_loop (_, none, Acc) ->
  Acc;
fold_loop (Fun, #nxt{pair = {Item, none}}, Acc) ->
  Fun(Item, Acc);
fold_loop (Fun, #nxt{pair = {Item, I}, 
                     oper = NewOper,
                     next = NewNext}, Acc) ->
  Acc1 = Fun(Item, Acc),
  try do_next(NewNext, NewOper, #repr{r = I}) of
      N = #nxt{} -> fold_loop(Fun, N, Acc1)
  catch
    exit:bad_iterator -> Acc1
  end.
      

foreach_loop (Next, Fun, Oper, R = #repr{}) ->
  try do_next(Next, Oper, R) of
      N = #nxt{} -> foreach_loop(Fun, N)
  catch
    exit:bad_iterator -> ok
  end.

foreach_loop (_, none) ->
  ok;
foreach_loop (Fun, #nxt{pair = {Item, none}}) ->
  Fun(Item),
  ok;  
foreach_loop (Fun, #nxt{pair = {Item, I},
                        oper = NewOper,
                        next = NewNext}) ->
  Fun(Item),
  try do_next(NewNext, NewOper, #repr{r = I}) of
      N = #nxt{} -> foreach_loop(Fun, N)
  catch
    exit:bad_iterator ->
      ok
  end.

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
  try do_next(Next, Oper, R) of
      N = #nxt{} -> all_loop(Fun, N)
  catch
    exit:bad_iterator -> true
  end.

all_loop (Fun, #nxt{pair = {Item, none}}) ->
  Fun(Item);
all_loop (Fun, #nxt{pair = {Item, I},
                    oper = NewOper,
                    next = NewNext}) ->
  case Fun(Item) of
    true   -> try do_next(NewNext, NewOper, #repr{r = I}) of
                  N = #nxt{} -> all_loop(Fun, N)
              catch
                exit:bad_iterator -> true
              end;
    false  -> false
  end.

any_loop (Next, Fun, Oper, R = #repr{}) ->
  try do_next(Next, Oper, R) of
      N = #nxt{} -> any_loop(Fun, N)
  catch
    exit:bad_iterator ->
      false
  end.
      
any_loop (Fun, #nxt{pair = {Item, none}}) ->
  Fun(Item);
any_loop (Fun, #nxt{pair = {Item, I},
                    oper = NewOper,
                    next = NewNext}) ->
  case Fun(Item) of
    false -> try do_next(NewNext, NewOper, #repr{r = I}) of
                 N = #nxt{} -> any_loop(Fun, N)
             catch
               exit:bad_iterator -> false
             end;
    true  -> true
  end.
