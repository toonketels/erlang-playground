-module(functions).

-export([head/1,second/1,same/2,valid_time/1,old_enough/1,right_age/1,oh_god/1,help_me/1,insert/2,beach/1]).

head([H|_]) -> H.

second([_,X|_]) -> X.

same(X,X) -> true;
same(_,_) -> false.

valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
    io:format("The date tuple (~p) says today is: ~p/~p/~p~n", [Date,Y,M,D]),
    io:format("The time tuple (~p) says now is: ~p:~p:~p~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Wrong format!~n").

old_enough(X) when X >= 18 -> true;
old_enough(_) -> false.

right_age(X) when X >= 18, X =< 106 -> true;
right_age(_) -> false.

oh_god(N) ->
    if N =:= 2 -> might_succeed;
    true -> always_does
end.

help_me(Animal) ->
    Talk = if Animal =:= cat-> 'meow';
              Animal =:= dog -> 'woof';
              Animal =:= fish -> 'blub';
              true -> 'bakjfkjdfkasjfd'
          end,
    io:format("Animal says ~s~n", [Talk]).

insert(X, []) ->
    [X];
insert(X, Set) ->
    case lists:member(X,Set) of
        true -> Set;
        false -> [X|Set]
    end.

beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kevin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in us only';
        _ ->
            'avoid beach'
    end.
