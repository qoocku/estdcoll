%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc TODO: Add description to i_collection_list_SUITE
%%% @end
%%% ==========================================================================
-module (i_collection_set_SUITE).
-extends (i_set_as_collection_suite).

-include_lib ("common_test/include/ct.hrl").

-export ([suite/0,
          all/0,
          groups/0, 
          init_per_group/2]).

-export([test_all/2,
         test_any/2,
         test_delete/2,
         test_filter/2,
         test_fold/2,
         test_foreach/2,
         test_map/2,
         test_put/2,
         test_has/2,
         test_is_empty/2]).

suite() -> 
  [{timetrap, {minutes,1}}].  

groups () ->
  ?BASE_MODULE:groups() 
    ++ [{sets, [parallel], [{group, set_as_collection}]}]
    ++ [{ordsets, [parallel], [{group, set_as_collection}]}].

init_per_group (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  Samples = Mod:from_list(lists:seq(1, 1000)),
  [{collection, i_set_set:new(Mod, Samples)},
   {samples, Samples},
   {erl_mod, Mod},
   {{specific, test_all}, fun test_all/2},
   {{specific, test_any}, fun test_any/2},
   {{specific, test_delete}, fun test_delete/2},
   {{specific, test_filter}, fun test_filter/2},
   {{specific, test_map}, fun test_map/2},
   {{specific, test_foreach}, fun test_foreach/2},
   {{specific, test_fold}, fun test_fold/2},
   {{specific, test_put}, fun test_put/2},
   {{specific, test_has}, fun test_has/2},
   {{specific, test_is_empty}, fun test_is_empty/2} | Config];
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group(Group, Config).

all() ->
    [{group, sets}, {group, ordsets}]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------

test_all (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, all, pred_fun,
                                 {std, std, fun (M, all, [P, D]) -> 
                                                lists:all(P, M:to_list(D))
                                            end}).

test_any (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, any, pred_fun,
                                 {std, std, fun (M, any, [P, D]) -> 
                                                lists:any(P, M:to_list(D))
                                            end}).

test_delete (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, delete, mutation_arg,
                               {std, 
                                fun (A, B, C) ->
                                    (apply(A, B, C)):to_erlang()
                                end, fun (M, delete, [V, D]) -> 
                                         M:del_element(V, D)
                                     end}).

test_filter (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, filter, pred_fun,
                               {std, 
                                fun (A, B, C) ->
                                    (apply(A, B, C)):to_erlang()
                                end, fun (M, filter, [P, D]) -> 
                                         M:filter(fun (V) -> P(V) end, D)
                                     end}).

test_fold (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, fold, fold_fun,
                                 {fun (F) -> [F, F(acc, ok)] end, 
                                  std,
                                  fun (M, fold, [F, A, D]) ->
                                    M:fold(fun (V, Acc) -> F(V, Acc) end, A, D)
                                  end}).

test_foreach (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  F = fun
        (M, foreach, [F, D]) ->
          put(acc, (?config(fold_fun, Config))(acc, ok)),
          lists:foreach(F, M:to_list(D)),
          get(acc);       
        (A, B, C) ->
          put(acc, (?config(fold_fun, Config))(acc, ok)),
          apply(A, B, C),
          get(acc)
      end,
  ?BASE_MODULE:test_collection(Config, foreach, foreach_fun, {std, F, F}).

test_put (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, put, mutation_arg,
                               {std,
                                fun (A, B, C) -> (apply(A, B, C)):to_erlang() end,
                                fun (M, put, [V, D]) -> M:add_element(V, D) end}).

test_has (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  Has = fun (M, has, [V, D]) ->
            M:is_element(V, D)
        end,
  ?BASE_MODULE:test_collection(Config, has, has_arg, {std, std, Has}).

test_is_empty (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, is_empty, undefined,
                               {std, std, fun (M, is_empty, [D]) -> M:size(D) == 0 end}).

test_map (Mod, Config) when Mod =:= sets orelse Mod =:= ordsets ->
  ?BASE_MODULE:test_collection(Config, map, trav_fun, 
                                 {std, 
                                  fun 
                                    (A, B, C) -> 
                                      D1 = (apply(A, B, C)):to_erlang(),
                                      lists:sort(Mod:to_list(D1)) 
                                  end,
                                  fun (M, map, [F, D]) -> lists:sort(lists:map(F, M:to_list(D))) end}).

  
