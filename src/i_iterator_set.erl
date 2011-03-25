%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-25
%%% @doc Erlang Standard Collection Iterators Library.
%%%      Iterators of sets implemented with `sets' and `ordsets' module.
%%% @end
%%% ==========================================================================
-module  (i_iterator_set).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

-opaque iterator () :: i_iterator_list:iterator().
-opaque repr     () :: i_iterator_list:repr().

-export_types ([iterator/0,
                repr/0]).

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

-spec new (set()) ->
              iterator();
          ({ordsets | sets, set()}) ->
              iterator().

new (Set) when is_list(Set) ->
  new({ordsets, Set});
new ({ordsets, Set}) when is_list(Set) ->
  i_iterator_list:new(Set);
new ({sets, Set}) when is_tuple(Set) ->
  i_iterator_list:new(sets:to_list(Set)).

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================
