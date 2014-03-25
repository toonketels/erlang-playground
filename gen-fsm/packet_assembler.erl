%
% http://www.erlang.org/doc/man/gen_fsm.html
%
-module(packet_assembler).
-behavior(gen_fsm).

-export([start/0, send_header/1, send_data/1]).
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4, init/1, terminate/3, waiting/2, collecting/2]).

-define(NAME, my_simple_packet_assembler).


% 
% Public api.
% 


% Start this baby
% 
start() ->
	gen_fsm:start_link({local, ?NAME}, ?MODULE, arg1, []).


% To send the length of a message
% 
send_header(Len) ->
	io:format("Packet assembler sending length~n"),
	gen_fsm:send_event(?NAME, Len).


% To send the message
% 
send_data(String) ->
	gen_fsm:send_event(?NAME, String).



% 
% Finite State Machine callback handlers.
% 

% On startup
% 
% Returns the initial stateName "waiting"
% and the associated stateData.
% 
init(arg1) ->
	io:format("Packet assembler starting~n"),
	{ok, waiting, nil}.


% On termination
%
% Accepts:
%   - Reason
%   - StateName
%   - StateData
% 
% Return value is ignored
% 
terminate(Reason, _, _) ->
	io:format("packet assembler terminated:"
		      "~p~n", [Reason]),
	true.


% On recieving any other message then the event messages
% 
% Can return with:
%   - stop: whcih will make gen_fsm call terminate
%   - next_state: to transition to a state
% 
handle_info(Info, StateName, StateData) ->
	io:format("packet assembler recieved info call:"
		      "~p~n", [Info]),
	{next_state, StateName, StateData}.


% On release upgrade/downgrade
% 
% Dummy implementation, does not work/
% 
% Accepts:
%   - OldVsn
%   - StateName
%   - StateData
%   - Extra: update instructions
% 
code_change(_, StateName, StateData, _) -> {StateName, StateData}.


% Whenever other processes want to communicate
% with our module whatever the state.
% 
% Dummy implementation: we just keep in the
% current state
% 
% Called on gen_fsm:send_all_state_event/2
% 
% When transitioning to a new state, the
% module:StateName is called.
% 
% Can return with:
%   - stop: whcih will make gen_fsm call terminate
%   - next_state: to transition to a state
% 
handle_event(Event, StateName, StateData) ->
	io:format("packet assembler recieved handle_event call:"
		      "~p~n", [Event]),
	{next_state, StateName, StateData}.


% On gen_fsm:sync_send_all_state_event/2,3
% 
% Basically whenever something wants to communicate with
% our fsm module no matter what state its in, and
% allows you to reply back.
% 
% Dummy implementation, we stay in current state.
% 
% Accepts:
%   - Event
%   - From
%   - StateName
%   - StateData
% 
% Can return with:
%   - stop: whcih will make gen_fsm call terminate
%   - next_state: to transition to a state
%   - reply: reply to calling code
% 
handle_sync_event(Event, _, StateName, StateData) ->
	io:format("packet assembler recieved hanle_sync_event call:"
		      "~p~n", [Event]),
	{next_state, StateName, StateData}.


%
% Our states
% 


% Waiting state.
% 
% We wait until we get a 'length' back.
% 
waiting(N, nil) ->
	io:format("Recieved message in waiting state~n"),
	{next_state, collecting, {N,0,[]}}.



% Collecting state.
% 
% We keep collecting until we have all the
% bytes.
% 
% Arguments
%   - Buff0: the buffer send to us
%   - Rest: arguments passed while transitioning between states
% 
collecting(Buff0, {Need, Length, Buff1}) ->
	L = length(Buff0),
	if
		L + Length < Need ->
			{next_state, collecting, {Need, L+Length, Buff1++Buff0}};
		L + Length == Need ->
			Buff = Buff1++Buff0,
			io:format("Got data:~s~n", [Buff]),
			{next_state, waiting, nil}
	end.