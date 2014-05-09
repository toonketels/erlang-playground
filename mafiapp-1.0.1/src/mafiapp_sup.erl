-module(mafiapp_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).

%
% Public interface.
%

start_link() ->
    supervisor:init(?MODULE, []).



%
% Supervisor callbacks.
%


% We dont really need a supervisor, we just use it as a synchronisation mechanism,
% it allows us to know when the tables are done.
init([]) ->
    {ok, {{one_for_one, 1, 1}, []}}.
