%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-19
%%% @doc TODO: Add description to i_iterator_gb_tree
%%% @end
%%% ==========================================================================
-module  (i_iterator_dict, [Keys, Dict, Oper]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/1, new/2]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([foreach/1,
          fold/2,
          next/0,
          map/1]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

new (D) ->
  new(D, fun (Item) -> Item end).

new (D, T) when is_function(T) ->
  instance(dict:fetch_keys(D), D, T).

new (Ks, D, T) when is_function(T) andalso is_list(Ks) ->
  instance(Ks, D, T).

next () when Keys =:= [] ->
  exit(bad_iterator);
next () ->
  Key = hd(Keys),
  {Oper({Key, dict:fetch(Key, Dict)}), instance(tl(Keys), Dict, Oper)}.

map (Fun) when is_function(Fun) ->
  new(Keys, Dict, Fun).

fold (Fun, Acc0) when is_function(Fun) ->
  fold_loop(Fun, Keys, Acc0).

foreach (Fun) when is_function(Fun) ->
  foreach_loop(Fun, Keys).

get (Key2) ->
  case dict:find(Key2) of
    error -> undefined;
    Other -> Other
  end.

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

fold_loop (_, [], Acc) ->
  Acc;
fold_loop (Fun, [K | Ks], Acc) ->
  fold_loop(Fun, Ks, Fun({K, dict:fetch(K, Dict)}, Acc)).

foreach_loop (_, []) ->
  ok;
foreach_loop (Fun, [K | Ks]) ->
  Fun({K, dict:fetch(K, Dict)}),
  foreach_loop(Fun, Ks).

get_loop (none, _) ->
  undefined;
get_loop ({K, V, _}, K) ->
  V;
get_loop ({_, _, I}, K) ->
  get_loop(gb_trees:next(I), K).
