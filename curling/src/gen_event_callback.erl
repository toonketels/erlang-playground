-module(gen_event_callback).
-behaviour(gen_event).

-export([code_change/3,handle_call/2,handle_event/2,handle_info/2,init/1,terminate/2]).


%
% gen_event callbacks
%

init([]) -> {ok, []}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok, State}.

handle_call(_,State) -> {ok, ok, State}.

handle_info(_,State) -> {ok, State}.

handle_event(_,State) -> {ok, State}.
