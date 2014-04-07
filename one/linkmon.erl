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
start_critic3() -> spawn(?MODULE, restarter2, []).

% Supervisor which 'resurect' a process in case it does abnormally.
% Since it restarts the process, it will have a different Pid.
%
% Register  to exposs the pid globally. In case the Pid dies, the atom will be
% removed and reregistered since restarter is called again.
%
% For a micromoment, the atom will not exist.
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

restarter2() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic2, []),
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
            restarter2()
    end.

% We add a Ref to each message. This is a unique identifier so we know the
% message is a response to our (they send the ref back, we pattern match on it).
% 
judge3(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic2() ->
    receive
        {From, Ref, {"Rage against the Turing machine", "Unit Testify"}} ->
            From ! {Ref, "They are great"};
        {From, Ref, {"System of a Downtime", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic2().
