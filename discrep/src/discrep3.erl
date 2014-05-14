-module(discrep3).
-export([run/0]).

run() ->
    Tup = money(5, you),
    % dializer wont catch this error...
    % We use type annotation in discrep4 to make it catch it.
    some_op(item(amount, Tup), item(account, Tup)).

money(Amount, Name) -> {give, Amount, Name}.

item(amount, {give, Amount,_}) -> Amount;
item(account, {give, _, Name}) -> Name.

some_op(A, B) -> A + B.
