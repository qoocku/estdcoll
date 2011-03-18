%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-17
%%% @doc TODO: Add description to i_collection_list_SUITE
%%% @end
%%% ==========================================================================
-module (i_collection_list_SUITE).
-extends (i_random_access_suite).

-include_lib ("common_test/include/ct.hrl").

-export ([suite/0,
          all/0,
          groups/0, 
          init_per_group/2]).

-export([test_put/2,
         test_size/2,
         test_has/2,
         test_is_empty/2]).

suite() -> 
  [{timetrap, {minutes,1}}].  

groups () ->
  ?BASE_MODULE:groups() ++ [{list, [parallel], [{group, random_access}]}].

init_per_group (list, Config) ->
  Samples = lists:seq(1, 1000),  
  [{collection, i_collection_list:new(Samples)},
   {samples, Samples},
   {erl_mod, lists},
   {{specific, test_put}, fun test_put/2},
   {{specific, test_size}, fun test_size/2},
   {{specific, test_has}, fun test_has/2},
   {{specific, test_is_empty}, fun test_is_empty/2} | Config];
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group(Group, Config).

all() ->
    [{group, list}]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------

test_put (lists, Config) ->
  ?BASE_MODULE:test_collection(Config, {put, append}, mutation_arg,
                               {std,
                                fun (A, B, C) -> (apply(A, B, C)):to_erlang() end,
                                fun (lists, append, [I, Xs]) -> [I | Xs] end}).

test_size (lists, Config) ->
  ?BASE_MODULE:test_collection(Config, size, undefined,
                               {std, std, fun (lists, size, [Xs]) -> length(Xs) end}).

test_has (lists, Config) ->
  ?BASE_MODULE:test_collection(Config, {has, member}, has_arg, std).

test_is_empty (lists, Config) ->
  ?BASE_MODULE:test_collection(Config, is_empty, undefined,
                               {std, std, fun (lists, is_empty, [Xs]) -> Xs =:= [] end}).
