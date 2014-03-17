% Server concurrency abstraction
% 
% Allows:
%  - starting/stopping server
%  - client sending messages to the server
%  - server to focus on handling a request
-module(server1).
-export([start/3,stop/1,rpc/2]).

% Starting the server.
% We can think of a server as a function: each
% request is the function being called
start(Name, F, State) ->
	register(Name,
			 spawn(fun() ->
			 			loop(Name, F, State)
			 	   end)).

stop(Name) -> Name ! stop.
	
rpc(Name, Query) ->
	Name ! {self(), Query},
	receive
		{Name, Reply} -> Reply
	end.

loop(Name, F, State) ->
	receive
		stop ->
			void;
		{Pid, Query} ->
			{Reply, State1} = F(Query, State),
			Pid ! {Name, Reply},
			loop(Name, F, State1)
	end.