-module(cards).
-export([main/0]).

% Type specifications...
-type suit() :: spades | hearts | clubs | diamonds.
-type value() :: 1..10 | j | q | k.
-type card() :: {suit(), value()}.

% We add spec to function so dialyzer can complain.
-spec kind(card()) -> 'face' | 'number'.
kind({_, A}) when A >= 1, A =< 10 -> number;
kind(_) -> face.


main() ->
    number = kind({spades, 10}),
    face = kind({hearts, q}),
    % This will make dialyzer complain, only when we add the -spec().
    number = kind({rubies, 2}),
    face = kind({clubs, k}).
