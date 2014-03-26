% 
% Playing with gen_server
% 
% Ch 6. Building an application - Armstrong thesis
% 
-module(kv).
-behavior(gen_server).

-export([start/0, stop/0, lookup/1, store/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

% 
% Client API
% 

% Instruct gen_server to startup with our code.
% 
% arg1 is passed to our init function. It should match.
% 
start() ->
	gen_server:start_link({local, kv}, kv, arg1, []).


% Instruct gen_server to stop our code.
% 
% We use cast to send stop signal, this
% is handled by us in handle_cast.
% 
stop() ->
	gen_server:cast(kv, stop).


% Instruct gen_server to call our code with
% {lookup, Key} message.
% 
% This is handled by us in handle_call.
% 
lookup(Key) ->
	gen_server:call(kv, {lookup, Key}).


% Instructs gen_server to call our code with
% {store, Key, Val} message.
% 
% Handled by us in handle_call.
% 
store(Key, Value) ->
	gen_server:call(kv, {store, Key, Value}).


% 
% Server API
% 

% Called by gen_server on startup
% We can add our initialization logic here.
% 
% We should return the initial application state.
% 
init(arg1) ->
	io:format("Key-Value server starting~n"),
	{ok, dict:new()}.


% Called by gen_server on shutdown.
% 
% We can add our termination logic.
% 
terminate(Reason, State) ->
	io:format("K-V server terminating~n").


% Called by gen_server on up/downgrade.
% 
% We shoukd update the state of our
% application and return it.
% 
code_change(OldVersion, State, Extra) ->
	{ok, State}.


% Called by gen_server on every "request".
% 
% Here goes our request handling logic.
% 
% Params:
%   - message
%   - from Pid
%   - state
% 
% Te reply we need to send a tuple:
%   - reply (atom)
%   - message to send to the client
%   - application state
% 
handle_call({store, Key, Val}, From, State) ->
	State1 = dict:store(Key, Val, State),
	{reply, ack, State1};
handle_call({lookup, crash}, From, State) ->
	1/0;  %% Will crash
handle_call({lookup, Key}, From, State) ->
	{reply, dict:find(Key, State), State}.

% Called by gen_server as "cast" request.
% 
% No idea what it means, just that its a special
% kind of request.
% 
handle_cast(stop, State) ->
	{stop, normal, State}.


% Called by gen_server as "info" request,
% whenever a timeout happens.
% 
% Same as "cast", a special request.
% 
handle_info(Info, State) ->
	{stop, normal, State}.