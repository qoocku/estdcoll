%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-19
%%% @doc TODO: Add description to i_iterator_gb_tree
%%% @end
%%% ==========================================================================
-module  (i_iterator_gb_tree, [Iter, Oper, Next]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/1, new/2, new/3]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([filter/1,
          filter_next/1,
          foreach/1,
          fold/2,
          next/0,
          map/1,
          partition/1]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

new (I) ->
  new(I, fun (Item) -> Item end).

new (I, T) when is_function(T) ->
  new(I, T, fun gb_trees:next/1).

new (I, T, N) when is_function(T) andalso is_function(N) ->
  instance(I, T, N).

next () when Iter =:= none ->
  exit(bad_iterator);
next () ->
  case Next(Iter) of
    {K, V, N} -> {Oper({K, V}), new(N, Oper, Next)};
    none      -> exit(bad_iterator)
  end.

map (Fun) when is_function(Fun) ->
  new(Iter, Fun, Next).

filter (Pred) when is_function(Pred) ->  
  new(Iter, Pred, fun filter_next/1).

fold (Fun, Acc0) when is_function(Fun) ->
  fold_loop(Fun, Iter, Acc0).

foreach (Fun) when is_function(Fun) ->
  foreach_loop(Fun, Iter).

partition (Fun) when is_function(Fun) ->
  {new(Iter, Fun), new(Iter, fun (I) -> not Fun(I) end)}.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

filter_next (none) ->
  none;
filter_next (I) ->
  {K, V, N} = gb_trees:next(I),
  case Oper({K, V}) of
    false -> filter_next(N);
    true  -> I
  end.

fold_loop (_, none, Acc) ->
  Acc;
fold_loop (Fun, {K, V, I}, Acc) ->
  fold_loop(Fun, gb_trees:next(I), Fun({K, V}, Acc)).

foreach_loop (_, none) ->
  ok;
foreach_loop (Fun, {K, V, I}) ->
  Fun({K, V}),
  foreach_loop(Fun, gb_trees:next(I)).