%%% ==========================================================================
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>
%%% @since 2011-03-18
%%% @doc Common Tests Base Features for `random_access' collections.
%%% @end
%%% ==========================================================================
-module  (i_random_access_suite).
-author  ("Damian T. Dobroczy\\'nski <qoocku@gmail.com> <email>").
-extends (i_collection_suite).

%%% --------------------------------------------------------------------
%%% C l i e n t  A P I  E x p o r t s
%%% --------------------------------------------------------------------

-include_lib ("common_test/include/ct.hrl").

-export ([all/0,
          groups/0, 
          init_per_group/2]).

%%% --------------------------------------------------------------------
%%% I n t e r n a l  e x p o r t s
%%% --------------------------------------------------------------------

-export ([test_at/1]).

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
  ?BASE_MODULE:groups() ++ [{random_access, [parallel], [{group, collections},
                                                           test_at]}].

init_per_group (random_access, Config) ->
  [{at_arg, 500} | Config];
init_per_group (Group, Config) ->
  ?BASE_MODULE:init_per_group(Group, Config).

all() ->
    [{group, random_access}]. 

%%% ============================================================================
%%% L o c a l  F u n c t i o n s
%%% ============================================================================

test_at (Config) ->
  ?BASE_MODULE:do_specific_test(test_at, Config).
