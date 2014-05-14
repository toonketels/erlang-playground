% Same as discrep3 but now with type annotations so
% dialyzer will catch the error.
-module(discrep4).
-export([run/0]).

-type cents() :: integer().
-type account() :: atom().
-type transaction() :: {'give', cents(), account()}.


run() ->
    Tup = money(5, you),
    some_op(item(amount, Tup), item(account, Tup)).

-spec money(cents(), account()) -> transaction().
money(Amount, Name) -> {give, Amount, Name}.

-spec item('amount', transaction()) -> cents();
           ('account', transaction()) -> account().
item(amount, {give, Amount,_}) -> Amount;
item(account, {give, _, Name}) -> Name.

some_op(A, B) -> A + B.
