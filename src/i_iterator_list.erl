%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-19
%%% @doc Erlang Standard Collection Iterators Library.
%%%      Iterators of collections implemented with `lists' module.
%%% @end
%%% ==========================================================================
-module  (i_iterator_list, [Type, Iter, Oper, Next]).
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
          filter_next/1,
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

new (L) when is_list(L) ->
  new(L, fun (Item) -> Item end).

new (L, T) when is_list(L) andalso is_function(T) ->
  instance(iterator, L, T, next_iter).

new (L, T, N) when is_function(T) andalso
                   is_list(L) andalso 
                   (is_atom(N) orelse
                                 (is_tuple(N) andalso size(N) == 2)) ->
  instance(iterator, L, T, N).

-define (IS_EMPTY_ITER(I), I =:= []).
-define (EMPTY_ITER_PATTERN, []).
-define (EMPTY_ITER, []).
-define (IMP_ALL, true).
-define (FILTER_NEXT_IMP, true).
-include ("estdcoll/src/estdcoll_iterator_imp.hrl").

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

-spec next_iter(repr()) -> repr().

next_iter (List) ->
  try
    {hd(List), tl(List)}
  catch
    _:_ ->
      none
  end. 
