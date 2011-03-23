%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc Erlang Standard Map as Collection Tests.
%%% @end
%%% ==========================================================================
-module (i_map_as_collection_suite).
-extends (i_random_access_and_strict_suite).

-include_lib ("common_test/include/ct.hrl").

-export ([all/0,
          groups/0, 
          init_per_group/2]).

groups () ->
  ?BASE_MODULE:groups() ++ [{map_as_collection, [parallel], [{group, random_access_and_strict}]}].

init_per_group (map_as_collection, Config) ->  
  [{mutation_arg, {-1000, 1000}},   
   {has_arg, {500, 500}},
   {foreach_fun, fun({_K, V}) -> put(acc, get(acc) + V) end},
   {pred_fun, fun ({K, V}) -> K =< 500 andalso V >= 300 end},
   {trav_fun, fun ({K, V}) -> {K*1234, V*123} end},   
   {fold_fun, fun 
                (acc, _)      -> 0;
                ({_, V}, Acc) -> Acc + V
              end} | Config];
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group(Group, Config).

all() ->
    [{group, map_as_collection}]. 

