-define (dbg(X), io:format("~p (~p): ~p\n", [?MODULE, ?LINE, X])).
-define (dbg(X, Y), io:format("~p (~p): ~p, ~p\n", [?MODULE, ?LINE, X, Y])).
-define (dbg(X, Y, Z), io:format("~p (~p): ~p, ~p, ~p\n", [?MODULE, ?LINE, X, Y])).
-define (dbg(X, Y, Z, J), io:format("~p (~p): ~p, ~p, ~p, ~p\n", [?MODULE, ?LINE, X, Y, Z])).
-define (dbg(X, Y, Z, J, K), io:format("~p (~p): ~p, ~p, ~p, ~p, ~p\n", [?MODULE, ?LINE, X, Y, Z, K])).
            
