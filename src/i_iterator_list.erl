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

-export ([all/1,
          any/1,
          dropwhile/1,
          dropwhile_next/1,
          foreach/1,
          next/0,
          next_iter/1,
          filter_next/1,
          fold/2,
          hd/0,
          head/0,
          map/1,
          partition/1,
          takewhile/1,
          takewhile_next/1,
          tl/0,
          tail/0,
          filter/1]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

-include_lib ("estdcoll/include/iterator.hrl").

-opaque iterator () :: module().
-opaque repr     () :: list().

-export_types ([iterator/0,
                repr/0]).

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

new (L) when is_list(L) ->
  new(L, fun (Item) -> Item end).

new (L, T) when is_list(L) andalso is_function(T) ->
  new(L, T, next_iter).

new (L, T, N) when is_list(L) andalso 
                   is_function(T) andalso 
                   (is_atom(N) orelse
                   (is_tuple(N) andalso size(N) == 2)) ->
  instance(iterator, if L =:= [] -> none ; true -> L end, T, N).

-define (IS_EMPTY_ITER(I), I =:= []).
-define (EMPTY_ITER_PATTERN, []).
-define (EMPTY_ITER, []).
-define (DROPWHILE_ALL_IMP, true).
-define (IMP_ALL, true).
-include ("estdcoll/src/estdcoll_iterator_imp.hrl").

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================


dropwhile (Pred) when Next =:= next_iter ->
  case lists:dropwhile(Pred, Iter) of
    [] ->
      none;
    Lst -> ?MODULE:new(Lst, Oper, Next)
  end;
dropwhile (Pred) ->
  default_dropwhile(Pred).
  
%% @doc Returns the first item referenced by the iterator. No availability check is done.

-spec head () -> any().

head () when Next =:= next_iter ->
  hd(Iter);
head () ->
  {Item, _} = next(),
  Item.

%% @doc Returns iterator referencing all but the first item. No availability check is done.

-spec tail () -> iterator().

tail () when Next =:= next_iter ->
  tl(Iter);
tail () ->
  {_, Tail} = next(),
  Tail.

-spec next_iter(repr()) -> repr().

next_iter (none) ->
  exit(bad_iterator);
next_iter (List) ->
  {hd(List), case tl(List) of [] -> none; L -> L end}. 
