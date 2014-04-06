-module(linkmon).
-compile(export_all).

myproc() ->
    timer:sleep(5000),
    exit(just_because).

and_again() ->
    io:format("Its me~n"),
    timer:sleep(1000),
    and_again().

% Spawn N processes and link them so see how they die togoter.
chain(0) ->
    timer:sleep(2000),
    io:format("Last one about to die~n"),
    exit(just_because);
chain(N) ->
    io:format("Creating a new one~n"),
    Pid = spawn(fun() -> chain(N-1) end),
    link(Pid),
    receive
        _ -> ok
    end.

% Same as above, except now we capture the exit signal so we can do stuff when a
% linked process dies. We're still going to die too.
chain_trapped(0) ->
    timer:sleep(5000),
    io:format("Last one about to die~n"),
    exit(just_because);
chain_trapped(N)->
    io:format("Creating a new one~n"),
    Pid = spawn(fun() -> chain_trapped(N-1) end),
    process_flag(trap_exit, true),
    link(Pid),
    receive
        Message ->
            io:format("Received a msg: ~p~n",[Message])
    end.
