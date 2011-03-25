%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-19
%%% @doc Erlang Standard Collection Iterators Library.
%%%      Iterators of maps implemented with `dict' module.
%%% @end
%%% ==========================================================================
-module  (i_iterator_dict).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([new/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

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

-spec new ({dict | orddict, dict()}) -> iterator();
          (dict()) -> iterator().
             
new ({DictType, Dict}) when DictType =:= dict orelse DictType =:= orddict ->
  i_iterator_list:new(DictType:to_list(Dict));
new (Dict) ->
  new({dict, Dict}).

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================
