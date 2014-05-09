-module(mafiapp_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1,end_per_suite/1,
          init_per_testcase/2,end_per_testcase/2,
          all/0]).
-export([add_service/1]).

% Run all tests
all() -> [add_service].


% Sets everything up to run the applciation
init_per_suite(Config) ->
    % Gets a new private directory on every run to store db in
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    % Specifies the db schema
    mafiapp:install([node()]),
    % Starts apps...
    application:start(mnesia),
    application:start(mafiapp),
    Config.


% Only stop the db.
end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_testcase(add_service, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.


%
% Specific tests

add_service(_Config) ->
    {error, unknown_friend} = mafiapp:add_service("form name", "to name", {1946,5,23}, "a fake service"),
    ok = mafiapp:add_friend("Don Corleone", [], [boss], boss),
    ok = mafiapp:add_friend("Alan Parsons",
                             [{twitter, "@alan"}],
                             [{born, {1982,11,11}},musician,'audio engineer', producer, "has projects"],
                             mixing),
    ok = mafiapp:add_service("Alan Parsons", "Don Corleone", {1946,5,23}, "helped releasing an album").
