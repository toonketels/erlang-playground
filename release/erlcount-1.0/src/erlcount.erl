% Main application.
%
% Starts the application supervisor.

-module(erlcount).
-behavior(application).
-export([start/2,stop/1]).

start(normal, _Args) ->
    erlcount_sup:start_link().

stop(_S) ->
    ok.
