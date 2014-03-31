-module(recur).

-export([factorial/1,len/1,len_t/1,tail_len/1,tail_factorial/1,duplicate/2,reverse/1,sublist/2,zip/2,lenient_zip/2,quicksort/1,qsort/1]).

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

% quicksort sorting algorithm.
%
% - Take an item (the pivot)
% - Go over all other items and divide them into a
%   smaller then pivot,larger then pivot group (partition).
% - Sort the smaller and larger groups (by calling itself on each group)
% - Once groups sorted, join in one lists
quicksort([]) ->
    [];
quicksort([Pivot|Rest]) ->
    {Smaller, Bigger} = partition(Pivot,Rest,[],[]),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Bigger).
partition(_,[],Smaller,Bigger) ->
    {Smaller,Bigger};
partition(Pivot,[H|T],Smaller,Bigger) ->
    if H =< Pivot -> partition(Pivot,T,[H|Smaller],Bigger);
       H > Pivot -> partition(Pivot,T,Smaller,[H|Bigger])
    end.

% A more optimized version:
% - uses tailrecursion (lower memory footprint)
% - equal to privots are not added to larger / smaller groups
%
% Even better: the pivot should be taken random
% to minimize risk of taken the highest/lowest value.
qsort(L) ->
    qsort(L,[]).
qsort([],Acc) ->
    Acc;
qsort([Pivot|Rest], Acc) ->
    part(Pivot,Rest,{[],[Pivot],[]},Acc).
part(_,[],{Smaller,Equal,Larger},Acc) ->
    qsort(Smaller,Equal++qsort(Larger,Acc));
part(Pivot,[H|T],{Smaller,Equal,Larger},Acc) ->
    if H < Pivot -> part(Pivot,T,{[H|Smaller],Equal,Larger},Acc);
       H > Pivot -> part(Pivot,T,{Smaller,Equal,[H|Larger]},Acc);
       H == Pivot -> part(Pivot,T,{Smaller,[H|Equal],Larger},Acc)
    end.
