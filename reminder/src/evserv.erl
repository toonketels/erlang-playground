%%% Event server for clients to connect to.
%%%
%%% It stores event information, spawns timers and notifies clients when timers
%%% are done.
-module(evserv).
-export([init/0]).

-record(state, {events,
                 clients}).

-record(event, {name="",
                 description="",
                 pid,
                 timeout={{1970,1,1},{0,0,0}}}).

%
%  Public api
%




%
% Internals
%

% Server initializer, should start the loop with a correct state.
%
% Both events and clients will be orddicts.
init() ->
    loop(#state{events=orddict:new(),
                clients=orddict:new()}).


loop(S = #state{}) ->
    receive
        % {Pid, MsgRef, {subscribe, Client}} -> todo;
        % {Pid, MsgRef, {add, Name, Description, Timeout}} -> todo;
        % {Pid, MsgRef, {cancel, Name}} -> todo;
        % {done, Name} -> todo;
        % shutdown -> todo;
        % {'DOWN', Ref, process, _Pid, _Reason} -> todo;
        % code_change -> todo;
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.
