-module(mafiapp_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1,end_per_suite/1,all/0]).


% Run all tests
all() -> [].


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
