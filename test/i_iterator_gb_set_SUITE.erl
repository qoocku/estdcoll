%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc Commont Tests for iterators implemented as lists.
%%% @end
%%% ==========================================================================
-module (i_iterator_gb_set_SUITE).
-extends (i_iterator_suite).

-include_lib ("common_test/include/ct.hrl").

-export ([suite/0,
          all/0,
          groups/0, 
          init_per_group/2]).

suite() -> 
  [{timetrap, {minutes,1}}].  

groups () ->
  ?BASE_MODULE:groups() ++
    [{gb_set, [parallel], [{group, iterators}]}].

init_per_group (gb_set, Config) ->
  Samples = gb_sets:from_list(lists:seq(1, 1000)),
  [{iterator, i_iterator_gb_set:new(Samples)},
   {samples, Samples},
   {erl_mod, gb_sets},
   {fold_fun, fun
                (acc, _) -> 0;
                (I, Acc) -> Acc + I 
              end},
   {foreach_fun, fun(Item) -> put(acc, get(acc) + Item) end},
   {pred_fun, fun (I) -> I =< 500 end},
   {trav_fun, fun (I) -> I + 1 end} | Config];
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group(Group, Config).

all() ->
    [{group, gb_set}]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------
