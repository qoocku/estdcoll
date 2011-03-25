-ifndef (ESTDCOLL_ITERATOR_IMP_HRL).
-define (ESTDCOLL_ITERATOR_IMP_HRL, true).

-ifdef (IMP_ALL).
-ifndef (HEAD_IMP).
-define (HEAD_IMP, true).
-endif.
-ifndef (NEXT_IMP).
-define (NEXT_IMP, true).
-endif.
-ifndef (MAP_IMP).
-define (MAP_IMP, true).
-endif.
-ifndef (FOREACH_IMP).
-define (FOREACH_IMP, true).
-endif.
-ifndef (FOLD_IMP).
-define (FOLD_IMP, true).
-endif.
-ifndef (ALL_IMP).
-define (ALL_IMP, true).
-endif.
-ifndef (ANY_IMP).
-define (ANY_IMP, true).
-endif.
-ifndef (FILTER_IMP).
-define (FILTER_IMP, true).
-endif.
-ifndef (TAIL_IMP).
-define (TAIL_IMP, true).
-endif.
-ifndef (PARTITION_IMP).
-define (PARTITION_IMP, true).
-endif.
-endif.


-ifdef (HEAD_IMP).

-compile ([{inline, [{hd, 0}]}]).

%% @doc Returns the first item referenced by the iterator.

-spec hd () -> any().

hd () when Iter =:= none ->
  exit(bad_iterator);
hd () ->
  head().

-endif.

-ifdef (NEXT_IMP).

%% @doc Shifts to the next-state iterator.

-spec next () -> iterator().

next () when ?IS_EMPTY_ITER(Iter) ->
  exit(bad_iterator);
next () ->
  {Mod, Fun} = case Next of
                 {M, F} -> {M, F};
                 F      -> {THIS, F}
               end,
  case estdcoll_iterators:do_next({Mod, Fun}, Oper, #repr{r = Iter}) of
    Last = {_, none} ->
      Last;
    {Item, I} ->
      {Item, new(I, Oper, Next)}
  end.

-endif.

-ifdef (ALL_IMP).

-spec all (b_collection:trav_fun()) -> iterator().

all (_) when ?IS_EMPTY_ITER(Iter) ->
  false;
all (Fun) when is_function(Fun) ->
  {Mod, Shift} = case Next of
                   {M, F} -> {M, F};
                   F      -> {THIS, F}
                 end,
  estdcoll_iterators:all_loop({Mod, Shift}, Fun, Oper, #repr{r = Iter}).

-endif.

-ifdef (ALL_IMP).

-spec any (b_collection:trav_fun()) -> iterator().

any (_) when ?IS_EMPTY_ITER(Iter) ->
  true;
any (Fun) when is_function(Fun) ->
  {Mod, Shift} = case Next of
                   {M, F} -> {M, F};
                   F      -> {THIS, F}
                 end,
  estdcoll_iterators:any_loop({Mod, Shift}, Fun, Oper, #repr{r = Iter}).

-endif.

-ifdef (MAP_IMP).

%% @doc Returns an iterator which next-state shift brings a value as 
%%      the result of `Fun' application on the underlaying item value.

-spec map (b_collection:trav_fun()) -> iterator().

map (Fun) when is_function(Fun) ->
  new(Iter, Fun, Next).

-endif.

-ifdef (FILTER_IMP).

%% @doc Returns an iterator which next-state function shifts to the collection
%%      nearest item `Item' for which `Pred(Item)' is `true'.

-spec filter (b_collection:filter_fun()) -> iterator().

filter (Pred) when is_function(Pred) ->  
  new(Iter, fun
              ({filter, Item}) -> Pred(Item);
              (Item) -> Oper(Item)
            end, filter_next).

-endif.

-ifdef (FOLD_IMP).

%% @doc Runs the iterator to reduce current and next items values to the one
%%      and only reult.

-spec fold (b_collection:fold_fun(), any()) -> any().
               
fold (Fun, Acc0) when is_function(Fun) ->
  {Mod, Shift} = case Next of
                   {M, F} -> {M, F};
                   F      -> {THIS, F}
                 end,
  estdcoll_iterators:fold_loop({Mod, Shift}, Fun, Oper, #repr{r = Iter}, Acc0).

-endif.

-ifdef (FOREACH_IMP).

%% @doc Returns an iterator for which next-state function calls `Fun' over
%%      its value.

-spec foreach (b_collection:foreach_fun()) -> ok.

foreach (Fun) when is_function(Fun) ->
  {Mod, Shift} = case Next of
                   {M, F} -> {M, F};
                   F      -> {THIS, F}
                 end,
  estdcoll_iterators:foreach_loop({Mod, Shift}, Fun, Oper, #repr{r = Iter}).

-endif.

-ifdef (PARTITION_IMP).

%% @equiv {filter(Pred), filter(fun (X) -> not Pred(X) end)}

-spec partition (b_collection:filter_fun()) -> {iterator(), iterator()}.

partition (Pred) when is_function(Pred) ->
  {filter(Pred), filter(fun (I) -> not Pred(I) end)}.

-endif.

-ifdef (TAIL_IMP).

%% @equiv begin {_, Tail} = Iter:next(), Tail end

-spec tl () -> iterator().

tl () when Iter =:= none ->
  exit(bad_iterator);
tl () ->
  tail().

-endif.

-ifdef (FILTER_NEXT_IMP).

filter_next (none) ->
  exit(bad_iterator);
filter_next (Iter) when ?IS_EMPTY_ITER(Iter) ->
  exit(bad_iterator);
filter_next (I) ->
  case next_iter(I) of
    ?EMPTY_ITER_PATTERN -> 
      none;
    Current = {Item, N} ->
      case Oper({filter, Item}) of
        false ->
          filter_next(N);
        true ->
          Current
      end
  end.

-endif.

-endif.




