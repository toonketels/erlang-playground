% Reverse Polish Notation Calculator
% (like Clojure in reverse).
%
% We use a stack to push items on.

% - read string
% - split on whitespace to create a list
%   (can be done manually by reading char by char of
%    of string untile we find a whitespace)
% - once we parse an operator:
%   - lookup arity
%   - pop elements of list for those arity
%   - apply funtion
%   - add result to 'stack'
% - continue until entire list is parsed
%   - if enough operants, we should have just one number
-module(calc).

-export([rpn/1,rpn_test/0]).

% Main worker "loop".
%
rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.


% Puts things on the stack and off again.
% To be used in the fold so only needs to
% return an updated Stack (accumulator).
%
rpn("+", [N1,N2|Stack]) -> [N2+N1|Stack];
rpn("-", [N1,N2|Stack]) -> [N2-N1|Stack];
rpn("*", [N1,N2|Stack]) -> [N2*N1|Stack];
rpn("/", [N1,N2|Stack]) -> [N2/N1|Stack];
rpn("^", [N1,N2|Stack]) -> [math:pow(N2,N1)|Stack];
rpn("ln", [N|Stack]) -> [math:log(N)|Stack];
rpn("log10", [N|Stack]) -> [math:log10(N)|Stack];
rpn("sum", Stack) -> [sum(Stack)];
rpn("prod", Stack) -> [prod(Stack)];
rpn(N, Stack) -> [read(N)|Stack].

sum(L) -> lists:foldl(fun(N, Acc) -> N + Acc end, 0, L).
prod(L) -> lists:foldl(fun(N, Acc) -> N * Acc end, 0, L).

% Reads a numberish string and tries
% to convert it to float/integer.
%
read(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F, _} -> F
    end.

rpn_test() ->
    5 = rpn("3 2 +"),
    6 = rpn("3 2 *"),
    50 = rpn("55 5 -"),
    4.0 = rpn("20 5 /"),
    ok.
