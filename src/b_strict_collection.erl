%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc TODO: Add description to b_random_access_collection
%%% @end
%%% ==========================================================================
-module  (b_strict_collection).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([behaviour_info/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (STRICT_COLLECTION_BEHAVIOR_EXPORTS, true).
-define (STRICT_COLLECTION_BEHAVIOR_SPECS, true).
-define (STRICT_COLLECTION_BEHAVIOR_TYPES, true).
-include ("estdcoll/include/strict_collection.hrl").

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
  estdcoll:inherit_behavior(b_collection, [{size, 0}]);
behaviour_info (_) ->
  undefined.

%%% ============================================================================
%%% B e h a v i o r  F u n c t i o n s
%%% ============================================================================

size () -> 0.
