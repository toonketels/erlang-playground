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

listen() ->
    receive
        _ ->
            io:format("Another message~n"),
            listen()
    end.

% Same as above, except now we capture the exit signal so we can do stuff when a
% linked process dies and not die automatically.
%
% To force kill another process do exit(Pid, kill)
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
            io:format("Received a msg: ~p~n",[Message]),
            exit(feel_like_it)
            % listen()
    end.


start_critic() -> spawn(?MODULE, critic, []).
start_critic2() -> spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    erlang:register(critic, Pid),
    receive
        {'EXIT', Pid, normal} ->
            io:format("Supervisor notices the critic died normally~n"),
            ok;
        {'EXIT', Pid, shutdown} ->
            io:format("Supervisor noticies the critic died because of shutdown~n"),
            ok;
        {'EXIT', Pid, _} ->
            io:format("Supervisor noticies the critic died without any good reason, Restarting..."),
            restarter()
    end.

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

judge2(Band, Album) ->
    critic ! {self(), {Band, Album}},
    Pid = whereis(critic),
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic() ->
    receive
        {From, {"Rage against the Turing machine", "Unit Testify"}} ->
            From ! {self(), "They are great"};
        {From, {"System of a Downtime", "Memoize"}} ->
            From ! {self(), "They're not Johnny Crash but they're good."};
        {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {self(), "Simply incredible."};
        {From, {_Band, _Album}} ->
            From ! {self(), "They are terrible!"}
    end,
    critic().
