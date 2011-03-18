%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc Standard Erlang Map as Collection Implemented-with-gb_tree Tests.
%%% @end
%%% ==========================================================================
-module (i_collection_gb_tree_SUITE).
-extends (i_map_as_collection_suite).

-include_lib ("common_test/include/ct.hrl").

-export ([suite/0,
          all/0,
          groups/0, 
          init_per_group/2]).

-export([test_all/2,
         test_any/2,
         test_at/2,
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
  ?BASE_MODULE:groups() ++ [{gb_tree, [parallel], [{group, map_as_collection}]}].

init_per_group (gb_tree, Config) ->
  Samples = gb_trees:from_orddict(lists:zip(lists:seq(1, 1000), lists:seq(1000, 1, -1))),
  [{collection, i_map_gb_tree:new(Samples)},
   {samples, Samples},
   {erl_mod, gb_trees},
   {{specific, test_all}, fun test_all/2},
   {{specific, test_any}, fun test_any/2},
   {{specific, test_delete}, fun test_delete/2},
   {{specific, test_filter}, fun test_filter/2},
   {{specific, test_map}, fun test_map/2},
   {{specific, test_foreach}, fun test_foreach/2},
   {{specific, test_fold}, fun test_fold/2},
   {{specific, test_put}, fun test_put/2},
   {{specific, test_has}, fun test_has/2},
   {{specific, test_is_empty}, fun test_is_empty/2},
   {{specific, test_at}, fun test_at/2} | Config];
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group(Group, Config).

all() ->
    [{group, gb_tree}]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------

test_all (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, all, pred_fun,
                                 {std, std, fun (gb_trees, all, [P, D]) -> 
                                                lists:all(P, gb_trees:to_list(D))
                                            end}).

test_any (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, any, pred_fun,
                                 {std, std, fun (gb_trees, any, [P, D]) -> 
                                                lists:any(P, gb_trees:to_list(D))
                                            end}).

test_at (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, at, at_arg, {std, std, fun (gb_trees, at, [K, D]) -> gb_trees:get(K, D) end}).

test_delete (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, delete, mutation_arg,
                               {std, 
                                fun (A, B, C) ->
                                    (apply(A, B, C)):to_erlang()
                                end, fun (gb_trees, delete, [{K, V}, D]) -> 
                                         case gb_trees:lookup(K, D) of
                                           {value, V} -> gb_trees:delete_any(K, D);
                                           {value, _} -> D;
                                           none -> D
                                         end
                                end}).

test_filter (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, filter, pred_fun,
                                 {std, 
                                  fun (A, B, C) ->
                                    X = apply(A, B, C),
                                    Y = X:to_erlang(),
                                    Z = gb_trees:to_list(Y),
                                    lists:sort(Z)
                                  end,
                                  fun (gb_trees, filter, [P, D]) ->
                                    L1 = gb_trees:to_list(D),
                                    L2 = lists:filter(P, L1),
                                    lists:sort(L2)
                                  end}).

test_fold (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, fold, fold_fun,
                                 {fun (F) -> [F, F(acc, ok)] end, 
                                  std,
                                  fun (gb_trees, fold, [F, A, D]) ->
                                    lists:foldl(F, A, gb_trees:to_list(D))
                                  end}).

test_foreach (gb_trees, Config) ->
  F = fun
        (gb_trees, foreach, [F, D]) ->
          put(acc, (?config(fold_fun, Config))(acc, ok)),
          lists:foreach(F, gb_trees:to_list(D)),
          get(acc);       
        (A, B, C) ->
          put(acc, (?config(fold_fun, Config))(acc, ok)),
          apply(A, B, C),
          get(acc)
      end,
  ?BASE_MODULE:test_collection(Config, foreach, foreach_fun, {std, F, F}).

test_put (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, put, mutation_arg,
                               {std,
                                fun (A, B, C) -> (apply(A, B, C)):to_erlang() end,
                                fun (gb_trees, put, [{K, V}, D]) ->
                                    try 
                                      gb_trees:insert(K, V, D)
                                    catch
                                      _:_ -> gb_trees:update(K, V, D)
                                    end
                                end}).

test_has (gb_trees, Config) ->
  Has = fun (gb_trees, has, [{K, V}, D]) ->
            case gb_trees:lookup(K, D) of
              none       -> false;
              {value, V} -> true;
              {value, _} -> false
            end
        end,
  ?BASE_MODULE:test_collection(Config, has, has_arg, {std, std, Has}).

test_is_empty (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, is_empty, undefined, std).

test_map (gb_trees, Config) ->
  ?BASE_MODULE:test_collection(Config, map, trav_fun, 
                                 {std, 
                                  fun (A, B, C) ->
                                      X = apply(A, B, C),
                                      Y = X:to_erlang(),
                                      Z = gb_trees:to_list(Y),
                                      lists:sort(Z) end,
                                  fun (gb_trees, map, [F, D]) -> lists:sort((lists:map(F, gb_trees:to_list(D)))) end}).

