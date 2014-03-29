-module(simple_logger).
-behaviour(gen_event).

-export([start/0, stop/0, log/1, report/0]).
-export([code_change/3, handle_call/2, handle_event/2, handle_info/2, init/1, terminate/2]).


-define(NAME, my_simple_event_logger).

% 
% Event managar api
% 


% Start our event manager.
% 
% Of event manager process was succesfully created,
% we immedatialy add our event handler.
% 
start() ->
	case gen_event:start_link({local, ?NAME}) of
		Ret = {ok, _} ->
			gen_event:add_handler(?NAME, ?MODULE, arg1),
			Ret;
		Other -> Other
	end.


stop() -> gen_event:stop(?NAME).


% Calling notify instructs gen_event to call
% handle_event on all event handlers.
% 
% gen_event holds the state for the event handler,
% so it passes it to the handler.
% 
log(E) -> gen_event:notify(?NAME, {log, E}).


% We can make direct request to a specific handler.
% Here we target only the handler defined by our
% module.
% 
% Notify targets all handlers.
% 
report() -> gen_event:call(?NAME, ?MODULE, report).



%
% Event handler api
% 


% Initialize event handler.
% 
init(arg1) ->
	io:format("Logger starting.~n"),
	{ok, []}.

% Called whenever gen_event asks us for a 'report'.
% We return our state as the report and provide the
% new state, which has not changed.
% 
% Args:
%  - Request
%  - State
% 
% Return:
%  - ok
%  - Reply
%  - new state
handle_call(report, S) -> {ok, S, S}.

% On log event, we create a new list with the
% event as the head, and our state (up to last 5
% messages) as the tail.
% 
% Args:
%  - Event
%  - State
% 
handle_event({log, E}, S) -> {ok, trim([E|S])}.


% Args:
%  - Info
%  - State
% 
handle_info(_, S) -> {ok, S}.

% Dummy implementation, not working...
% 
code_change(_, S, _) -> {ok, S}.


% Shutdown
%  - Argument
%  - State
% 
terminate(stop, _) -> true.

% We are only interested in the last five messages,
% so we keep only 5 as our state.
% 
trim([X1,X2,X3,X4,X5|_]) -> [X1,X2,X3,X4,X5];
trim(L) -> L.