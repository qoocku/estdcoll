%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc TODO: Add description to i_collection_list_SUITE
%%% @end
%%% ==========================================================================
-module (i_collection_list_SUITE).

-include_lib ("common_test/include/ct.hrl").

-export ([suite/0,
          all/0,
          groups/0, 
          init_per_suite/1,
          end_per_suite/1,
          init_per_group/2,
          end_per_group/2,
          init_per_testcase/2,
          end_per_testcase/2]).

-export([test_foreach/1,
         test_fold/1,
         test_map/1,
         test_filter/1,
         test_any/1,
         test_all/1,
         test_put/1,
         test_delete/1,
         test_size/1,
         test_reverse/1]).

suite() -> 
  [{timetrap, {minutes,1}}].  

groups () ->
  [{collections, [sequence], [test_foreach,
                              test_fold,
                              test_map,
                              test_filter,
                              test_any,
                              test_all,
                              test_put,
                              test_delete]},
   {ordered, [sequence], [{group, collections},
                          test_size,
                          test_reverse]},
   {list, [sequence], [{group, ordered}]}].

init_per_suite(Config) -> 
    Config. 

end_per_suite (_Config) ->    
    ok.

init_per_group (list, Config) ->
  Samples = lists:seq(1, 1000),  
  [{collection, i_collection_list:new(Samples)},
   {samples, Samples},
   {erl_mod, lists},
   {erl_put, append} | Config];
init_per_group (_, Config) ->
  Config.

end_per_group (_, _) ->
  ok.

init_per_testcase(_Case, Config) ->
  [{mutation_arg, -1000},
   {foreach_fun, fun(Item) -> put(acc, get(acc) + Item) end},
   {pred_fun, fun (I) -> I =< 500 end},
   {trav_fun, fun (I) -> I + 1 end},
   {fold_fun, fun
                (acc, _) -> 0;
                (I, Acc) -> Acc + I 
              end} | Config].

end_per_testcase(_Case, _Config) -> 
    ok. 

all() ->
    [{group, list}]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------

test_foreach (Config) ->
  F = fun (A, B, C) ->
          put(acc, 0),
          apply(A, B, C),
          get(acc)
      end,
  test_collection(Config, foreach, foreach_fun, {std, F, F}).

test_fold (Config) ->
  test_reduce(Config, {fold, foldl}).

test_map (Config) ->
  test_traverse(Config, map).

test_filter (Config) ->
  test_collection(Config, filter, pred_fun,
                  {std, fun (A, B, C) -> (apply(A, B, C)):to_erlang() end, std}).

test_all (Config) ->
  test_predicate(Config, all).

test_any (Config) ->
  test_predicate(Config, any).

test_put (Config) ->
  test_put(?config(erl_mod, Config), Config).

test_put(lists, Config) ->
  test_collection(Config, {put, append}, mutation_arg,
                  {std,
                   fun (A, B, C) -> (apply(A, B, C)):to_erlang() end,
                   fun (lists, append, [I, Xs]) -> [I | Xs] end}).

test_delete (Config) ->
  test_mutation(Config, delete).

test_size (Config) ->
  test_size(?config(erl_mod, Config), Config).

test_size (lists, Config) ->
  test_collection(Config, size, undefined,
                  {std, std, fun (lists, size, [Xs]) -> length(Xs) end}).

test_reverse (Config) ->
  test_fun0(Config, reverse).

%%% ------- local functions --------

test_collection (Config, Function, FunKey, ResultFun) ->
  {Fun1, Fun2} = case Function of
                   {F1, F2} -> {F1, F2};
                   Function -> {Function, Function}
                 end,
  {AFun,
   RFun1, RFun2} = case ResultFun of
                     std           -> {fun (X) -> [X] end, fun apply/3, fun apply/3};
                     {std, std, Fc}-> {fun (X) -> [X] end, fun apply/3, Fc};                     
                     {std, Fb, std}-> {fun (X) -> [X] end, Fb, fun apply/3};
                     {std, Fb, Fc} -> {fun (X) -> [X] end, Fb, Fc};   
                     {Fa, std, std}-> {Fa, fun apply/3, fun apply/3}; 
                     {Fa, std, Fc} -> {Fa, fun apply/3, Fc};
                     {Fa, Fb, std} -> {Fa, Fb, fun apply/3};
                     {Fa, Fb, Fc}  -> {Fa, Fb, Fc};
                     ResultFun     -> {fun (X) -> [X] end, ResultFun, ResultFun}
                   end,
  Collection = ?config(collection, Config),
  Samples    = ?config(samples, Config),
  ErlMod     = ?config(erl_mod, Config),
  {Result,
   Expected} = case ?config(FunKey, Config) of
                 undefined ->
                   {RFun1(Collection, Fun1, []),
                    RFun2(ErlMod, Fun2, [Samples])};
                 Fun ->
                   {RFun1(Collection, Fun1, AFun(Fun)),
                    RFun2(ErlMod, Fun2, AFun(Fun) ++ [Samples])}
               end,
  Result     = Expected.
  
test_predicate (Config, Function) ->
  test_collection(Config, Function, pred_fun, std).

test_traverse (Config, Function) ->
  test_collection(Config, Function, trav_fun, 
                  {std, fun (A, B, C) -> (apply(A, B, C)):to_erlang() end, std}).

test_reduce (Config, Function) ->
  test_collection(Config, Function, fold_fun,
                  {fun (F) -> [F, F(acc, ok)] end, std, std}).

test_mutation (Config, Function) ->
  test_collection(Config, Function, mutation_arg,
                  {std, fun (A, B, C) -> (apply(A, B, C)):to_erlang() end, std}).

test_fun0 (Config, Function) ->
  test_collection(Config, Function, undefined,
                  {std, fun (A, B, C) -> (apply(A, B, C)):to_erlang() end, std}).
