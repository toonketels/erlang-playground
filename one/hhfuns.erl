-module(hhfuns).

-export([one/0,two/0,add/2,increment/1,decrement/1,apply_list/2,filter/2,fold/2,cfold/3,freverse/1,fmap/2]).

one() -> 1.
two()  -> 2.

% Must be invoked by
%   hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
%
add(X,Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

% Use it like
%   hhfuns:apply_list([1,2,3,4,4], fun(V) -> V+1 end).
%
% Should be called map and args swapped (fun first)
%
apply_list([], _) -> [];
apply_list([H|T], F) -> [F(H)|apply_list(T, F)].


filter(Predicate, L) ->
    filter(Predicate, lists:reverse(L), []).
filter(_, [], Acc) ->
    Acc;
filter(Predicate, [H|T], Acc) ->
    case Predicate(H) of
        true -> filter(Predicate, T, [H|Acc]);
        false -> filter(Predicate, T, Acc)
    end.

% We dont provide a starting value like in the book
% but always use the first and second value for folding.
%
% Better to provide a starting value as the fun passed
% to fold might alter the value before accumulating it.
fold(_, []) -> [];
fold(_, [H|[]]) -> H;
fold(Reducer, [First|[Second|T]]) -> fold(Reducer, T, Reducer(First, Second)).
fold(_, [], Acc) -> Acc;
fold(Reducer, [H|T], Acc) -> fold(Reducer, T, Reducer(H, Acc)).

% correct fold (better) with starting value.
cfold(_, Start, []) -> Start;
cfold(Reducer, Start, [H|T]) -> cfold(Reducer, Reducer(H,Start), T).

freverse(L) ->
    % Event his breaks because we dont have  starting value
    % fold(fun(X, Acc) -> [X|Acc] end, L).
    cfold(fun(X, Acc) -> [X|Acc] end, [], L).

fmap(F, L) ->
    % This breaks when we pass a list of one
    % becaus we cant provide a starting value.
    % lists:reverse(fold(fun(X, Acc) -> [F(X)|Acc] end, L)).
    lists:reverse(cfold(fun(X, Acc) -> [F(X)|Acc] end, [], L)).
