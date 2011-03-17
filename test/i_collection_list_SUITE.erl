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
          init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2]).

-export([test_foreach/1]).

suite() -> 
    [{timetrap, {minutes,1}}].  

init_per_suite(Config) -> 
    Config. 

end_per_suite (_Config) ->    
    ok.
 
init_per_testcase(_Case, Config) ->
  [{collection, i_collection_list:new([])},
   {samples,    []} | Config].

end_per_testcase(_Case, _Config) -> 
    ok. 

all() ->
    [test_foreach]. 

%% --------------------------------------------------------------------
%% TEST CASES
%% --------------------------------------------------------------------

test_foreach (Config) ->
  Collection = ?config(collection, Config),
  Samples    = ?config(samples, Config),
  io:format("Collection: ~p\nSamples = ~p\n", [Collection, Samples]),
  put(acc, 0),
  Collection:foreach(F = fun(Item) -> put(acc, get(acc) + Item) end),
  Result = get(acc),
  put(acc, 0),
  lists:foreach(F, Samples),
  Result = get(acc).