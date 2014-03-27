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
% Returns the shape of the supervisor tree and
% strategy used:
%   - one_for_one: or
%   - 5, 1000: terminate after 5 retries in
%              1000 seconds (so stop retrying and exit)
%              the restart frequency
%   - list of processes to start with for each:
%      - identifier (for the supervisor only)
%      - startup callback and args
%      - permanent: will be auto restarted on failure
%      - 500: arg passed to process on shutdown.
%             it gives the process 500 miliseconds
%             to stop its activities before restarting it.
%      - its' a worker process and not a simple_supervisor
%      - the module
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
