%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-19
%%% @doc Erlang Standard Collection Iterators Library.
%%%      Iterator on a set implemented as `gb_set'.
%%%      
%%%      == Modules parameters ==
%%%      
%%%      <ul>
%%%      <li> `Type' - `iterator' atom </li>
%%%      <li> `Iter' - value of type `gb_trees:iterator()' </li>
%%%      <li> `Oper' - function of type `transfer_fun()' </li>
%%%      <li> `Next' - next-state function definition of type `shift_fun_def()' </li>
%%%      </ul>
%%% @end
%%% ==========================================================================
-module  (i_iterator_gb_set, [Type, Iter, Oper, Next]).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/1,
          new/2,
          new/3]).

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

-type transfer_fun  () :: fun((any()) -> any()).
-type shift_fun_def () :: atom() | {atom(), atom()}.
-type repr          () :: gb_sets:iterator().
-opaque iterator    () :: module().

-export_type ([iterator/0]).

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

%% @doc Creates an iterator using `gb_trees:iterator/0' value. The transfer operation
%%      is equal by default to identity function `fun (X) -> X end'.
%% @equiv new(I, fun (X) -> X end, {gb_trees, next})

-spec new (repr()) -> module().
              
new (I) ->
  new(I, fun (Item) -> Item end).

%% @doc Creates an iterator using `gb_trees:iterator/0' value. The transfer operation
%%      is defined by `T' value and next-state shift function is defined as
%%      `gb_trees:next/1'.
%% @equiv new(I, fun (X) -> X end, {gb_trees, next})

-spec new (repr(), transfer_fun()) -> iterator().

new (I, T) when is_function(T) ->
  new(I, T, {gb_sets, next}).

%% @doc Creates an iterator using `gb_trees:iterator/0' value. The transfer operation
%%      is defined by `T' value and the next-state shift function is `N'.

-spec new (repr(), transfer_fun(), shift_fun_def()) -> iterator().

new (I, T, N) when is_function(T) andalso 
                   (is_atom(N) orelse is_tuple(N)) ->
  instance(iterator, I, T, N).

-define (IS_EMPTY_ITER(I), I =:= none).
-define (EMPTY_PATTERN, none).
-define (EMPTY_ITER_PATTERN, none).
-define (IMP_ALL, true).
-include ("estdcoll/src/estdcoll_iterator_imp.hrl").

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

filter_next (none) ->
  none;
filter_next (I) ->
  case gb_sets:next(I) of
    Current = {Item, N} -> case Oper({filter, Item}) of
                             false -> filter_next(N);
                             true  -> Current
                           end;
    none -> none
  end.