-module(discrep2).
-export([run/0]).

run() ->
    Tup = money(5, you),
    some_op(count(Tup), account(Tup)).

money(Amount, Name) -> {give, Amount, Name}.

count({give,Amount,_}) -> Amount.

account({give, _, Name}) -> Name.


some_op(A, B) -> A + B.
