-module(list_com).

-export([nums/0, strings/0, even/0, combined/0, deep/0]).

nums() ->
    X = lists:seq(1, 20),
    print(X).

strings() ->
    X = ["one", "two", "three"],
    print(X).

even() ->
    L = [X || X <- [1, 2, 3, 4, 5, 6, 7, 8], X rem 2 =:= 0],
    print(L).

combined() ->
    L = [X * Y || X <- [1,2,3,4], Y <- [11, 12, 13]],
    io:format("Total items: ~p~n", [length(L)]),
    print(L).

deep() ->
    X = [{blue, color}, {green, color}, {round, shape}, {square, shape}],
    L = [A || {A, B} <- X, B =:= color],
    print(L).

print([H|T]) ->
    io:format("~p~n", [H]),
    print(T);
print(I) ->
    io:format("~p~n", [I]),
    ok.
