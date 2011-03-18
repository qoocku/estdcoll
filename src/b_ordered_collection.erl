%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc Erlang Standard Ordered Collection Behavior. 
%%% @end
%%% ==========================================================================
-module  (b_ordered_collection).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([behaviour_info/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (ORDERED_COLLECTION_BEHAVIOR_SPECS, true).
-define (ORDERED_COLLECTION_BEHAVIOR_TYPES, true).
-define (ORDERED_COLLECTION_BEHAVIOR_EXPORTS, true).
-include ("estdcoll/include/random_access_collection.hrl").
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

-spec behaviour_info (callbacks) -> estdcoll:behavior_list().

behaviour_info (callbacks) ->
  Mine = [{append,  1},
          {foldl,   2},
          {foldr,   2},
          {prepend, 1},
          {reverse, 0}],
  estdcoll:inherit_behavior([b_random_access_collection,
                             b_strict_collection], Mine);
behaviour_info (_) ->
  undefined.

%%% ============================================================================
%%% B e h a v i o r  F u n c t i o n s
%%% ============================================================================

append (_) -> ?MODULE.
foldl (_, _) -> {}.
foldr (_, _) -> {}.
prepend (_) -> ?MODULE.
reverse () -> ?MODULE.

