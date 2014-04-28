% Main application supervisor.
%
% Will start the dispatch module.

-module(erlcount_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).

%
% Public interface
%

start_link() ->
    supervisor:start_link(?MODULE, []).

%
% supervisor callbacks
%

% Starts the erlcount_dispatch module with start_link func without args, named
% internally 'dispatch'.
init([]) ->
    MaxRestart = 5,
    MaxTime = 100,
    {ok, {{one_for_one, MaxRestart, MaxTime},
          [{dispatch,
            {erlcount_dispatch, start_link, []},
            transient,
            100000,
            worker,
            [erlcount_dispatch]}]}}.
