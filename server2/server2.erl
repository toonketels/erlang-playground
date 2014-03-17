% Server concurrency abstraction
% 
% Makes the server fault tolerant by
%  - retstarting the server on a crash with the old state
%  - killing the client when it makes an illegal rpc request
%  - killing the client when the server takes too long to respond
%    For inctance, when the server is shut down and the client is
%    is waiting for a response.
%
-module(server2).
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
		{Name, crash} -> exit(rpc);
		{Name, ok, Reply} -> Reply
	after 10000 ->
		exit(timeout)
	end.

loop(Name, F, State) ->
	receive
		stop ->
			void;
		{From, Query} ->
			case (catch F(Query, State)) of
				{'EXIT', Why} ->
					log_error(Name, Query, Why),
					From ! {Name, crash},
					loop(Name, F, State);
				{Reply, State1} ->
					From ! {Name, ok, Reply},
					loop(Name, F, State1)
			end
	end.

log_error(Name, Query, Why) ->
	io:fwrite("Error occured!~n", []),
	io:format("Server ~p query ~p caused exeption ~p~n",
		[Name, Query, Why]).