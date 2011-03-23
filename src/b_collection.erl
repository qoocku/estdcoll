%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @since 2011-03-17
%%% @doc Erlang Standard Generic Collection Behavior.
%%% @end
%%% ==========================================================================
-module(b_collection).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com>").
-include ("vsn").

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([behaviour_info/1]).

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

-define (COLLECTION_BEHAVIOR_EXPORTS, true).
-define (COLLECTION_BEHAVIOR_TYPES, true).
-define (COLLECTION_BEHAVIOR_SPECS, true).
-include ("estdcoll/include/collection.hrl").

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

behaviour_info (callbacks) ->
  [{all,       1},
   {any,       1},          
   {delete,    1},
   {extend,    1},
   {filter,    1},      
   {fold,      2},
   {foreach,   1},
   {has,       1},
   {internals, 0},
   {iterator,  0},
   {isa,       1},
   {is_empty,  0},
   {map,       1},
   {merge,     2},
   {put,       1},
   {to_erlang, 0}];
behaviour_info (_) ->
  undefined.

%%% ============================================================================
%%% I n t e r f a c e  F u n c t i o n s
%%% ============================================================================

any (Pred) when is_function(Pred) -> true.
all (Pred) when is_function(Pred) -> true.
delete (_) -> ?MODULE.
extend (_) -> ?MODULE.
filter (Pred) when is_function(Pred)  -> ?MODULE.
fold (Fun, _) when is_function(Fun)   -> ok.
foreach (Fun) when is_function(Fun)   -> ok.
has (_)      -> true.
internals () -> {}.
iterator () -> ?MODULE.
is_empty ()  -> true.
isa (Type) when is_atom(Type) -> true.
map (Fun) when is_function(Fun)      -> ?MODULE.
merge (Fun, _) when is_function(Fun) -> ?MODULE.
put (_) -> ?MODULE.
to_erlang () -> {}.

