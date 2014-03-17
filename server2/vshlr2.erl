% Very Simple Home Location Register
%
% What we get is basically a client which can
% start/stop the server and ask and set locations.
-module(vshlr2).
-export([start/0, stop/0, handle_event/2, i_am_at/2, find/1]).

-import(server2, [start/3,stop/1,rpc/2]).
-import(dict, [new/0, store/3, find/2]).

% 
% Client commands...
% 

start() -> start(vshlr, fun handle_event/2, new()).

stop() -> stop(vshlr).

i_am_at(Who, Where) ->
	rpc(vshlr, {i_am_at, Who, Where}).

find(Who) ->
	rpc(vshlr, {find, Who}).


% 
% Server handling of incoming rpc calls...
% 

handle_event({i_am_at, Who, Where}, Dict) ->
	{ok, store(Who, Where, Dict)};
handle_event({find, "robert"}, Dict) ->
	% Cause an error by div by zero...
	1/0;
handle_event({find, Who}, Dict) ->
	{find(Who, Dict), Dict}.