% Main supervisor.
%
% Starts and stops pools by controlling the pool specific supervisor.
%
-module(ppool_supersup).
-behaviour(supervisor).
-export([start_link/0,stop/0,start_pool/3,stop_pool/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

% killing supervisors is not easy, so we're brutal about it.
%
stop() ->
    case whereis(ppool) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.

start_pool(Name,Limit,MFA) ->
    ChildSpec = {Name,
                 {ppool_sup, start_link, [Name, Limit, MFA]},
                 permanent, 10500, supervisor, [ppool_sup]},
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).

% Start it without children
%
init([]) ->
    MaxRestarts = 6,
    MaxTime = 3600,
    {ok, {{ok, one_for_one, MaxRestarts, MaxTime}, []}}.
