%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc TODO: Add description to i_collection_list_SUITE
%%% @end
%%% ==========================================================================
-module (i_collection_dict_SUITE).
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
  ?BASE_MODULE:groups() ++ [{dict, [parallel], [{group, map_as_collection}]}].

init_per_group (dict, Config) ->
  Samples = dict:from_list(lists:zip(lists:seq(1, 1000), lists:seq(1000, 1, -1))),
  [{collection, i_map_dict:new(Samples)},
   {samples, Samples},
   {erl_mod, dict},
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
    [{group, dict}]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------

test_all (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, all, pred_fun,
                                 {std, std, fun (dict, all, [P, D]) -> 
                                                lists:all(P, dict:to_list(D))
                                            end}).

test_any (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, any, pred_fun,
                                 {std, std, fun (dict, any, [P, D]) -> 
                                                lists:any(P, dict:to_list(D))
                                            end}).

test_at (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, at, at_arg, {std, std, fun (dict, at, [K, D]) -> dict:fetch(K, D) end}).

test_delete (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, delete, mutation_arg,
                               {std, 
                                fun (A, B, C) ->
                                    (apply(A, B, C)):to_erlang()
                                end, fun (dict, delete, [{K, V}, D]) -> 
                                         case dict:find(K, D) of
                                           {ok, V} -> dict:erase(K, D);
                                           error -> D
                                         end
                                end}).

test_filter (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, filter, pred_fun,
                                 {std, 
                                  fun (A, B, C) ->
                                    (apply(A, B, C)):to_erlang()
                                  end, fun (dict, filter, [P, D]) -> 
                                          dict:filter(fun (K, V) -> P({K, V}) end, D)
                                        end}).

test_fold (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, fold, fold_fun,
                                 {fun (F) -> [F, F(acc, ok)] end, 
                                  std,
                                  fun (dict, fold, [F, A, D]) ->
                                    dict:fold(fun (K, V, Acc) -> F({K, V}, Acc) end, A, D)
                                  end}).

test_foreach (dict, Config) ->
  F = fun
        (dict, foreach, [F, D]) ->
          put(acc, (?config(fold_fun, Config))(acc, ok)),
          lists:foreach(F, dict:to_list(D)),
          get(acc);       
        (A, B, C) ->
          put(acc, (?config(fold_fun, Config))(acc, ok)),
          apply(A, B, C),
          get(acc)
      end,
  ?BASE_MODULE:test_collection(Config, foreach, foreach_fun, {std, F, F}).

test_put (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, put, mutation_arg,
                               {std,
                                fun (A, B, C) -> (apply(A, B, C)):to_erlang() end,
                                fun (dict, put, [{K, V}, D]) -> dict:store(K, V, D) end}).

test_has (dict, Config) ->
  Has = fun (dict, has, [{K, V}, D]) ->
            case dict:find(K, D) of
              error -> false;
              {ok, V} -> true;
              {ok, _} -> false
            end
        end,
  ?BASE_MODULE:test_collection(Config, has, has_arg, {std, std, Has}).

test_is_empty (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, is_empty, undefined,
                               {std, std, fun (dict, is_empty, [D]) -> dict:size(D) == 0 end}).

test_map (dict, Config) ->
  ?BASE_MODULE:test_collection(Config, map, trav_fun, 
                                 {std, 
                                  fun (A, B, C) -> (apply(A, B, C)):to_erlang() end,
                                  fun (dict, map, [F, D]) -> dict:from_list(lists:map(F, dict:to_list(D))) end}).

  