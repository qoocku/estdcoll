%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-19
%%% @doc Erlang Standard Collection Iterators Library.
%%%      Iterators of maps implemented with `dict' module.
%%% @end
%%% ==========================================================================
-module  (i_iterator_dict, [Type, Iter, Oper, Next]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/1, new/2, new/3]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([foreach/1,
          next/0,
          next_iter/1,
          fold/2,
          map/1,
          partition/1,
          filter/1]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

-opaque iterator () :: module().
-type repr     () :: {[any()], dict()}.

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

new (D) ->
  new(D, fun (Item) -> Item end).

new (D, T) when is_function(T) ->
  instance(iterator, {dict:fetch_keys(D), D}, T, next_iter).

new ({Ks, D}, T, N) when is_function(T) andalso
                         is_list(Ks) andalso 
                         is_atom(N) andalso
                         size(N) == 2 ->
  instance(iterator, {Ks, D}, T, N).

-define (GET_ITER(I), element(1, I)).
-define (EMPTY_ITER, []).
-define (IMP_ALL, true).
-include ("estdcoll/src/estdcoll_iterator_imp.hrl").

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

-spec next_iter(repr()) -> {any(), repr()}.

next_iter ({Keys, Dict}) ->
  {dict:fetch(hd(Keys), Dict), {tl(Keys), Dict}}.
