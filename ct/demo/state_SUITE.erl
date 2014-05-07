-module(state_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0,init_per_testcase/2,end_per_testcase/2]).
-export([ets_tests/1]).

%
% Tip: use ct:pal like io:format to output to html and shell
% 

all() -> [ets_tests].

% First arg is the test name. So we can have multiple init_per_testcase
% clauses pattern matching individual tests.
% Do init_per_testcase(_, Config) to target all unmatched tests.
init_per_testcase(ets_tests, Config) ->
    TabId = ets:new(account, [ordered_set, public]),
    ets:insert(TabId, {andy, 2131}),
    ets:insert(TabId, {david, 12}),
    ets:insert(TabId, {steve, 127654321}),
    [{table, TabId} | Config].

end_per_testcase(ets_tests, Config) ->
    ets:delete(?config(table, Config)).

ets_tests(Config) ->
    TabId = ?config(table, Config),
    [{david, 12}] = ets:lookup(TabId, david),
    steve = ets:last(TabId),
    true = ets:insert(TabId, {zachary, 99}),
    zachary = ets:last(TabId).
