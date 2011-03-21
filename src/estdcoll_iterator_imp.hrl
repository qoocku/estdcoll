-ifndef (ESTDCOLL_ITERATOR_IMP_HRL).
-define (ESTDCOLL_ITERATOR_IMP_HRL, true).

-ifdef (IMP_ALL).
-define (NEXT_IMP, true).
-define (MAP_IMP, true).
-define (FOREACH_IMP, true).
-define (FOLD_IMP, true).
-define (FILTER_IMP, true).
-define (PARTITION_IMP, true).
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
  case stdcoll_iterator:do_next({Mod, Fun}, Oper, Next, Iter) of
    {Item, I} ->
      {Item, new(I, Oper, Next)};
    none -> exit(bad_iterator)
  end.

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
  stdcoll_iterator:fold_loop({Mod, Shift}, Fun, Oper, Iter, Acc0).

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
  stdcoll_iterator:foreach_loop({Mod, Shift}, Fun, Oper).

-endif.

-ifdef (PARTITION_IMP).

%% @equiv {filter(Pred), filter(fun (X) -> not Pred(X) end)}

-spec partition (b_collection:filter_fun()) -> {iterator(), iterator()}.

partition (Pred) when is_function(Pred) ->
  {filter(Pred), filter(fun (I) -> not Pred(I) end)}.

-endif.

-ifdef (FILTER_NEXT_IMP).

filter_next (_, Iter) when ?IS_EMPTY_ITER(Iter) ->
  ?EMPTY_ITER;
filter_next (Oper, I) ->
  case next_iter(I) of
    Current = {{K, V}, N} ->
      case Oper({filter, {K, V}}) of
        false ->
          filter_next(Oper, N);
        true ->
          Current
      end;
    None = ?EMPTY_ITER_PATTERN -> None
  end.

-endif.

-endif.
