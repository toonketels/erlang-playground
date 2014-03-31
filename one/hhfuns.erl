-module(hhfuns).

-export([one/0,two/0,add/2,increment/1,decrement/1,apply_list/2]).

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
