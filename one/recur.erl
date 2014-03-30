-module(recur).

-export([factorial/1,len/1,len_t/1,tail_len/1,tail_factorial/1,duplicate/2,reverse/1,sublist/2,zip/2,lenient_zip/2]).

factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

% space grows linear.
%
% the more items, the more memore used.
%
% Operations are stacked, and finally reduced.
len([]) ->
    0;
len([_|T]) ->
    1 + len(T).

% Tail recursion, this is in constant space.
%
% We reduce immediatly intead of at the end.
len_t([]) ->
    0;
len_t([_|T]) ->
    len_t(1, T).
len_t(N, []) ->
    N;
len_t(N, [_|T]) ->
    len_t(N + 1, T).

% A better implementation of above.
tail_len(L) ->
    tail_len(L, 0).
tail_len([], N) ->
    N;
tail_len([_|T], N) ->
    tail_len(T, N + 1).

% All tail recursions needs an accumulator.
%
tail_factorial(N) ->
    tail_factorial(N, 1).
tail_factorial(0, A) ->
    1 * A;
tail_factorial(N, A) when N > 0 ->
    tail_factorial(N - 1, N * A).

duplicate(Times, Item) ->
    duplicate(Times, Item, []).
duplicate(0, _Item, Acc) ->
    Acc;
duplicate(Times, Item, Acc) when Times > 0 ->
    duplicate(Times - 1, Item, [Item|Acc]).

reverse(L) ->
    reverse(L, []).
reverse([], Rl) ->
    Rl;
reverse([H|T], Rl) ->
    reverse(T, [H|Rl]).

% Since tail recursion build lists in reverse,
% we reverse the list first to get the items
% in the correct order.
%
% lists:reverse is an optimized BiF because
% its very often used.
sublist(L,N) ->
    reverse(sublist(L,N,[])).
sublist([],_N,Sl) ->
    Sl;
sublist(_L,0,Sl) ->
    Sl;
sublist([H|T],N,Sl) when N > 0 ->
    sublist(T,N-1,[H|Sl]).

zip(L1,L2) ->
    zip(L1, L2, []).
zip([],[], Zl) ->
    reverse(Zl);
zip([H1|T1], [H2|T2], Zl) ->
    zip(T1, T2, [{H1,H2}|Zl]).

lenient_zip(L1,L2) ->
    lenient_zip(L1, L2, []).
lenient_zip([],_, Zl) ->
    reverse(Zl);
lenient_zip(_,[],Zl) ->
    reverse(Zl);
lenient_zip([H1|T1], [H2|T2], Zl) ->
    lenient_zip(T1, T2, [{H1,H2}|Zl]).
