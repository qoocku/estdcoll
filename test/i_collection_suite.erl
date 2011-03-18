%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-18
%%% @doc Erlang Standard Collection Generic Collection Tests Suite Utilities.
%%% @end
%%% ==========================================================================
-module  (i_collection_suite).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").

-include_lib ("common_test/include/ct.hrl").

-export ([all/0,
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
         test_has/1,
         test_is_empty/1]).

-export([test_foreach/2,         
         test_fold/2,
         test_map/2,
         test_filter/2,
         test_any/2,
         test_all/2,
         %test_put/2,
         test_delete/2
         %test_has/2,
         %test_is_empty/2
         ]).


-export ([test_collection/4,
          test_fun0/2,
          test_mutation/2,
          test_predicate/2,
          test_reduce/2,
          test_traverse/2,
          do_specific_test/2,
          do_specific_test/3]).

groups () ->
  [{collections, [parallel], [test_foreach,
                              test_fold,
                              test_map,
                              test_filter,
                              test_any,
                              test_all,
                              test_put,
                              test_delete,
                              test_has,
                              test_is_empty]}].

init_per_suite(Config) -> 
    Config. 

end_per_suite (_Config) ->    
    ok.

init_per_group (collections, Config) ->
  [{mutation_arg, -1000},
   {has_arg, 500},
   {foreach_fun, fun(Item) -> put(acc, get(acc) + Item) end},
   {pred_fun, fun (I) -> I =< 500 end},
   {trav_fun, fun (I) -> I + 1 end},
   {fold_fun, fun
                (acc, _) -> 0;
                (I, Acc) -> Acc + I 
              end} | Config];
init_per_group (_, Config) ->
  Config.

end_per_group (_, _) ->
  ok.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_Case, _Config) -> 
    ok. 

all() ->
    [{group, collections}]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------

test_foreach (Config) ->
  do_specific_test(test_foreach, Config).

test_foreach (_, Config) ->
  F = fun (A, B, C) ->
          put(acc, 0),
          apply(A, B, C),
          get(acc)
      end,
  test_collection(Config, foreach, foreach_fun, {std, F, F}).

test_fold (Config) ->
  do_specific_test(test_fold, Config).

test_fold (_, Config) ->
  test_reduce(Config, {fold, foldl}).

test_map (Config) ->
  do_specific_test(test_map, Config).

test_map (_, Config) ->
  test_traverse(Config, map).

test_filter (Config) ->
  do_specific_test(test_filter, Config).

test_filter (_, Config) ->
  test_collection(Config, filter, pred_fun,
                  {std, fun (A, B, C) -> (apply(A, B, C)):to_erlang() end, std}).

test_all (Config) ->
  do_specific_test(test_all, Config).

test_all (_, Config) ->
  test_predicate(Config, all).

test_any (Config) ->
  do_specific_test(test_any, Config).

test_any (_, Config) ->
  test_predicate(Config, any).

test_put (Config) ->
  do_specific_test(test_put, Config).

test_delete (Config) ->
  do_specific_test(test_delete, Config).

test_delete (_, Config) ->
  test_mutation(Config, delete).

test_size (Config) ->
  do_specific_test(test_size, Config).

test_has (Config) ->
  do_specific_test(test_has, Config).

test_is_empty (Config) ->
  do_specific_test(test_is_empty, Config).

%%% ------- local functions --------

do_specific_test (Test, Config) ->
  do_specific_test(Test, Config, fun (_, Cfg) -> ?MODULE:Test(undefined, Cfg) end).

do_specific_test (Test, Config, Default) ->
  ErlMod = ?config(erl_mod, Config),
  case ?config({specific, Test}, Config) of
    undefined -> Default(ErlMod, Config);
    Fun       -> Fun(ErlMod, Config)
  end.

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
