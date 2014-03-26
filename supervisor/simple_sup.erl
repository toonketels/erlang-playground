-module(simple_sup).
-behaviour(supervisor).

-export([start/0, init/1]).

-define(NAME, simple_supervisor).

% Our api to start the supervisor
%
start() ->
    supervisor:start_link({local, ?NAME}, ?MODULE, nil).


% Called by supervisor on startup (start_link/3)
% to inform it on restart strategy and such.
%
init(_) ->
    {ok,
        {{one_for_one, 5, 1000},
         [{packet,
           {packet_assembler, start, []},
           permanent, 500, worker, [packet_assembler]},
          {server,
           {kv, start, []},
           permanent, 500, worker, [kv]},
          {logger,
           {simple_logger, start, []},
           permanent, 500, worker, [simple_logger]}]}}.
