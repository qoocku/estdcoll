%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc TODO: Add description to b_random_access_collection
%%% @headerfile "estdcoll/include/random_access_collection.hrl"
%%% @end
%%% ==========================================================================
-module  (b_random_access_collection).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([behaviour_info/1]).

-define (USE_SKELETONS, true).
-define (USE_SPECS, true).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (RANDOM_ACCESS_COLLECTION_BEHAVIOR_EXPORTS, true).
-define (RANDOM_ACCESS_COLLECTION_BEHAVIOR_SPECS, true).
-define (RANDOM_ACCESS_COLLECTION_BEHAVIOR_TYPES, true).
-include ("estdcoll/include/random_access_collection.hrl").

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

%% @doc Returns callbacks needed by implementation of this type of collections.

-spec behaviour_info (callbacks) -> estdcoll:behavior_list().

behaviour_info (callbacks) ->
  estdcoll:inherit_behavior(b_collection, [{at, 1}, {get, 1}]);
behaviour_info (_) ->
  undefined.

%%% ============================================================================
%%% B e h a v i o r  F u n c t i o n s
%%% ============================================================================

%% @doc Gets an item specified by given key. The key may be an index (for indexed sequences
%%      like lists) or any other term (for mappings). If the item does not exists in the collections
%%      the function exits with `badarg' value.

at (Key) -> {}.

get (Key) -> any.

