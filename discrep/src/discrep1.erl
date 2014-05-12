-module(discrep1).
-export([run/0]).

run() -> some_ops(5, you).

some_ops(A, B) -> A+B.
