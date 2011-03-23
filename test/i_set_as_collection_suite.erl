%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc Erlang Standard Map as Collection Tests.
%%% @end
%%% ==========================================================================
-module (i_set_as_collection_suite).
-extends (i_strict_suite).

-include_lib ("common_test/include/ct.hrl").

-export ([all/0,
          groups/0, 
          init_per_group/2]).


groups () ->
  ?BASE_MODULE:groups() ++ [{set_as_collection, [parallel], [{group, strict}]}].

init_per_group (set_as_collection, Config) ->  
  [{mutation_arg, -1000},   
   {has_arg, 500},
   {foreach_fun, fun(V) -> put(acc, get(acc) + V) end},
   {pred_fun, fun (V) -> V >= 300 end},
   {trav_fun, fun (V) -> V*123 end},   
   {fold_fun, fun 
                (acc, _)      -> 0;
                (V, Acc) -> Acc + V
              end} | Config];
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group(Group, Config).

all() ->
    [{group, set_as_collection}]. 
