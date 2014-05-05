-module(m8ball).
-behaviour(application).
-export([ask/1]).
-export([start/2,stop/1]).

%
% Public interface
%
ask(Question) ->
    m8ball_server:ask(Question).



%
% Application callbacks
%


start(normal, []) ->
    m8ball_sup:start_link().

stop(_State) ->
    ok.
