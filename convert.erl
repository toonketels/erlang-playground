-module(convert).
-export([main/0]).

main() ->
    [_,_] = convert({a,b}),
    {_,_} = convert([a,b]),
    {_,_} = convert({a,b}),
    [_,_] = convert([a,b]).


% We need this specific spec otherwise dializer assumes a spec such as:
% `spec(convert list() | tuple()) -> list() | tuple()`
% which wont catch the error.
-spec convert(tuple()) -> list();
              (list()) -> tuple().
convert(Tup) when is_tuple(Tup) -> tuple_to_list(Tup);
convert(L = [_|_]) -> list_to_tuple(L).
