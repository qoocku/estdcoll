%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-18
%%% @doc TODO: Add description to i_ordered_suite
%%% @end
%%% ==========================================================================
-module  (i_ordered_suite).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-extends (i_collection_suite).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-include_lib ("common_test/include/ct.hrl").

-export ([all/0,
          groups/0]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([test_size/1,
          test_size/2,
          test_reverse/1]).

%%% --------------------------------------------------------------------
%%% M a c r o s
%%% --------------------------------------------------------------------

%%% --------------------------------------------------------------------
%%% R e c o r d s ,  T y p e s  a n d  S p e c s
%%% --------------------------------------------------------------------

%%% ============================================================================
%%% C l i e n t  A P I / E x p o r t e d  F u n c t i o n s
%%% ============================================================================

groups () ->
  ?BASE_MODULE:groups() ++ [{ordered, [parallel], [{group, collections},
                                                     test_size,
                                                     test_reverse]}].

all() ->
    [{group, ordered}]. 

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

test_size (Config) ->
  test_size(?config(erl_mod, Config), Config).

test_size (lists, Config) ->
  ?BASE_MODULE:test_collection(Config, size, undefined,
                                 {std, std, fun (lists, size, [Xs]) -> length(Xs) end}).

test_reverse (Config) ->
  ?BASE_MODULE:test_fun0(Config, reverse).
