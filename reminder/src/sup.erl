% Supervisor to restart process when they die.

-module(sup).
-export([start/2,start_link/2,init/1,loop/1]).
-behavior(supervisor).

start(Module,StartupArgs) ->
    spawn(?MODULE, init, [{Module, StartupArgs}]).

start_link(Module,StartupArgs) ->
    spawn_link(?MODULE, init, [{Module, StartupArgs}]).

init({Module, StartupArgs}) ->
    process_flag(trap_exit, true),
    loop({Module, start_link, StartupArgs}).

loop({Module, StartupFunc, StartupArgs}) ->
    Pid = apply(Module, StartupFunc, StartupArgs),
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n.", [Pid, Reason]),
            loop({Module, StartupFunc, StartupArgs})
    end.
