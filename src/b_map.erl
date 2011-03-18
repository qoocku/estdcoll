%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc Erlang Standard Map Behavior. 
%%% @end
%%% ==========================================================================
-module  (b_map).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([behaviour_info/1]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-define (MAP_BEHAVIOR_TYPES, true).
-define (MAP_BEHAVIOR_EXPORTS, true).
-define (MAP_BEHAVIOR_SPECS, true).
-include ("estdcoll/include/map.hrl").

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
  Mine = [{put,  2}],
  estdcoll:inherit_behavior([b_collection,
                             b_random_access_collection,
                             b_strict_collection], Mine);
behaviour_info (_) ->
  undefined.

%%% ============================================================================
%%% B e h a v i o r  F u n c t i o n s
%%% ============================================================================

put (_, _) -> ?MODULE.
map_values (_) -> ?MODULE.
fetch (_) -> ?MODULE.
