-module(dolphins).
-compile(export_all).

% Concurrency basics
%
% Spawn a new process:
%   Pid = spawn(fn)
%   Pid = spawn(module, fn, []).
%
% Send a message
%   Pid ! "hello"
%
% Send your own pid so the other process can respond
%   Pid ! {self(), "hello"}
%
% Read all messages in your inbox (shell)
%   flush().
%
% Spawned process listening for messages.
%    fn() ->
%        receive ->
%            {Pid, _} -> Pid ! "Back at you!",
%            fn()
%        end.


% Outputs a string whenever a given message is recieved and exits.
dolphin1() ->
    receive
        do_a_flip ->
            io:format("How about no~n");
        fish ->
            io:format("So long and thanks for the fish!~n");
        _ ->
            io:format("Hey, we are smarter then humans~n")
    end.

dolphin2() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no";
        {From, fish} ->
            From ! "So long and thanks for the fish!";
        _ ->
            io:format("Hey, we are smarter then humans~n")
    end.

dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for the fish!";
            % This one exits...
        _ ->
            io:format("Hey, we are smarter then humans~n"),
            dolphin3()
    end.
