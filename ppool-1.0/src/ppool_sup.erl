% a pool supervisor.
%
-module(ppool_sup).
-export([start_link/3]).
-export([init/1]).
-behaviour(supervisor).

start_link(Name,Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
    MaxRestarts = 1,
    MaxTime = 3600,
    {ok, {{one_for_all, MaxRestarts, MaxTime},
          [{serv,
            {ppool_serv, start_link, [Name, Limit, self(), MFA]},
            permanent, 5000, worker, [ppool_serv]}]}}.
