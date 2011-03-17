%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc Erlang Standard Collection Library Utils.
%%% @end
%%% ==========================================================================
-module  (estdcoll).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([inherit_behavior/2,
          inherit_behaviour/2]).

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

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

-define (_IB_, (module(), behavior_list()) -> behavior_list();
               ([module()], behavior_list()) -> behavior_list()). 
-type behavior_list () :: [{atom, non_neg_integer()}].
-spec inherit_behavior ?_IB_.
-spec inherit_behaviour ?_IB_. 

inherit_behavior (Parent, Mine) when is_atom(Parent) ->
  inherit_behavior ([Parent], Mine);
inherit_behavior (Parents, Mine) ->
  sets:to_list(sets:from_list(lists:flatten([P:behaviour_info(callbacks) || P <- Parents]) ++ Mine)).

inherit_behaviour (Parents, Mine) ->
  inherit_behavior(Parents, Mine).

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

