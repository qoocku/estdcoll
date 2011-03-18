%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-18
%%% @doc TODO: Add description to i_random_access_and_strict_suite
%%% @end
%%% ==========================================================================
-module  (i_random_access_and_strict_suite).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-extends (i_random_access_suite).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-export ([all/0,
          groups/0, 
          init_per_group/2]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export([test_size/1]).

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
  ?BASE_MODULE:groups() ++
    i_strict_suite:groups() ++
    [{random_access_and_strict, [parallel], [{group, random_access},
                                             {group, strict}]}].   

all () ->
  [{group, random_access_and_strict}].

init_per_group (strict, Config) ->
  i_strict_suite:init_per_group(strict, Config);
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group (Group, Config).

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

test_size (Config) ->
  i_strict_suite:test_size(Config).
