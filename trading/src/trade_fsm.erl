-module(trade_fsm).
-behavior(gen_fsm).

-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).

%
% Public interface
%



%
% Implementation
%

init(_Args) -> todo.

terminate(_Reason, _StateName, _StateData) -> todo.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> todo.
    %  {ok, NextStateName, NewStateData}

% To handle global events, those send via gen_fsm:send_all_state_event/2
%
handle_event(_Event, _StateName, _StateData) -> todo.

handle_sync_event(_Event, _From, _StateName, _StateData) -> todo.

handle_info(_Info, _StateName, _StateData) -> todo.
