% Application startup module.
%
% Defines how to start and stop our app.
-module(simple).
-behavior(application).

-export([start/2, stop/1]).

% Starting means to start our top supervisor.
% 
start(_Type, _Args) ->
    simple_sup:start().

stop(_State) ->
    ok.
