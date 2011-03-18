%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc Erlang Standard Collection Library Utils.
%%% @end
%%% ==========================================================================
-module  (estdcoll).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([inherit_behavior/2,
          inherit_behaviour/2,
          register_merge/2,
          merge/2]).

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
-export_type ([behavior_list/0]).

%% @doc Flattens the behavior "inheritance" chain.

-spec inherit_behavior ?_IB_.

inherit_behavior (Parent, Mine) when is_atom(Parent) ->
  inherit_behavior ([Parent], Mine);
inherit_behavior (Parents, Mine) ->
  sets:to_list(sets:from_list(lists:flatten([P:behaviour_info(callbacks) || P <- Parents]) ++ Mine)).

%% @equiv inherit_behavior/2
-spec inherit_behaviour ?_IB_. 

inherit_behaviour (Parents, Mine) ->
  inherit_behavior(Parents, Mine).

%% @doc Registers callback for merging collection of type `T1' and type `T2'.

-spec register_merge ({atom(), atom()}, fun ((any(), b_collection:collection()) -> b_collection:collection())) -> ok.

register_merge ({T1, T2}, MergeFun) ->
  exit(not_implemented).

merge (Coll1, Coll2) ->
  exit(not_implemented).


%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

