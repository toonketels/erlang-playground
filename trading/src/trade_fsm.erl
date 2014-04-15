-module(trade_fsm).
-behavior(gen_fsm).

% Public API
-export([start/1,start_link/1,trade/2,accept_trade/1,make_offer/2,retract_offer/2,ready/1,cancel/1]).
% gen_fsm callbacks
-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).
% custom state names
-export([idle/2,idle/3,idle_wait/2,idle_wait/3,negotiate/2,negotiate/3,wait/2,ready/2,ready/3]).

%
% Public API
%

start(_) -> todo.

start_link(_) -> todo.

trade(_,_) -> todo.

accept_trade(_) -> todo.

make_offer(_,_) -> todo.

retract_offer(_,_) -> todo.

ready(_) -> todo.

cancel(_) -> todo.



%
% Custom state names
%

idle(_,_) -> todo.

idle(_,_,_) -> todo.

idle_wait(_,_) -> todo.

idle_wait(_,_,_) -> todo.

negotiate(_,_) -> todo.

negotiate(_,_,_) -> todo.

wait(_,_) -> todo.

ready(_,_) -> todo.

ready(_,_,_) -> todo.



%
% gen_fsm callbacks
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
