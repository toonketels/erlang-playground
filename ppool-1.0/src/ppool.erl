% Interface module, provides an easy api
% to work with the ppool.
-module(ppool).
-export([start/2,stop/1,start_pool/3,stop_pool/1,run/2,sync_queue/2,async_queue/2]).

% Starts the ppool application, now as an OTP app
%
start(normal, _Args) ->
    ppool_supersup:start_link().

% Stops the entire ppool application
%
stop(_State) ->
    ppool_supersup:stop().

% Starts a new pool.
% The MFA specifies how the workers should be supervised in the pool aka how
% to start a worker (and so,what worker to use for that pool).
%
start_pool(Name, Limit, {M,F,A}) ->
    ppool_supersup:start_pool(Name, Limit, {M,F,A}).

% Stops a given pool
%
stop_pool(Name) ->
    ppool_supersup:stop_pool(Name).

% Args specify the values to pass for that worker process.
%
run(Name, Args) ->
    ppool_serv:run(Name, Args).

sync_queue(Name, Args) ->
    ppool_serv:sync_queue(Name, Args).

async_queue(Name, Args) ->
    ppool_serv:async_queue(Name, Args).
