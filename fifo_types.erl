% Queues.
%
% A queue is created by using two stacks. One for adding items, one for removing
% items (both as stacks == LIFO). When the second stack (removing items) is empty,
% the two switch places (first one used to remove items from, second one to add).
% This is more performant then using one structure for adding and removing.
-module(fifo_types).
-export([new/0,push/2,pop/1,empty/1]).
-export([test/0]).

-spec new() -> {fifo, [], []}.
new() -> {fifo, [], []}.

-spec push({fifo, In::list(), Out::list()}, term()) -> {fifo, list(), list()}.
push({fifo, In, Out}, X) -> {fifo, [X|In], Out}.

% Consume an element of a queue.
-spec pop({fifo, In::list(), Out::list()}) -> {term(), {fifo, list(), list()}}.
pop({fifo, [], []}) -> erlang:error('empty fifo');
pop({fifo, In, []}) -> pop({fifo, [], lists:reverse(In)});
pop({fifo, In, [X|Out]}) -> {X, {fifo, In, Out}}.

% Its better is_empty()
-spec empty({fifo, [], []}) -> true;
            ({fifo, nonempty_list(), nonempty_list()}) -> false.
empty({fifo, [], []}) -> true;
empty({fifo, _In, _Out}) -> false.


test() ->
    N = new(),
    {4, N2} = pop(push(push(new(), 4), 8)),
    {8, N3} = pop(N2),
    N = N3,
    true = empty(N3),
    false = empty(N2),
    {e, _} = pop({fifo, [a,b], [e]}).
