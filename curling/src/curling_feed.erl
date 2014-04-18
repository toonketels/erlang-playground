-module(curling_feed).
-behaviour(gen_event).
-export([init/1,terminate/2,code_change/3,handle_call/2,handle_info/2,handle_event/2]).

% Argument passed by calling gen_event:add_handler/3
%
init([Pid]) -> {ok, Pid}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok, State}.

handle_call(_,State) -> {ok, ok, State}.

handle_info(_,State) -> {ok, State}.

% The state is second param which is the Pid in our case
%
handle_event(Event, Pid) ->
    Pid ! {curling_feed, Event},
    {ok, Pid}.
