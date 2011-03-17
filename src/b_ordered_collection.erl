%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc Erlang Standard Ordered Collection Behavior. 
%%% @end
%%% ==========================================================================
-module  (b_ordered_collection).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([behaviour_info/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (ORDERED_COLLECTION_BEHAVIOR_INT, true).
-define (ORDERED_COLLECTION_BEHAVIOR_IMP, true).
-include ("estdcoll/include/ordered_collection.hrl").

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

behaviour_info (callbacks) ->
  Mine = [{at,      1},
          {append,  1},
          {foldl,   2},
          {foldr,   2},
          {prepend, 1},
          {reverse, 0}],
  estdcoll:inherit_behavior(b_collection, Mine);
behaviour_info (_) ->
  undefined.

%%% ============================================================================
%%% B e h a v i o r  F u n c t i o n s
%%% ============================================================================

at (_) -> item.
append (_) -> ?MODULE.
foldl (_, _) -> {}.
foldr (_, _) -> {}.
prepend (_) -> ?MODULE.
reverse () -> ?MODULE.

