% Server concurrency abstraction
% 
% Enable code reload without restart.
%
-module(server3).
-export([start/3,stop/1,rpc/2,swap_code/2]).

% Starting the server.
% We can think of a server as a function: each
% request is the function being called
start(Name, F, State) ->
	register(Name,
			 spawn(fun() ->
			 			loop(Name, F, State)
			 	   end)).

stop(Name) -> Name ! stop.

swap_code(Name, F) -> rpc(Name, {swap_code, F}).

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
		{From, {swap_code, F1}} ->
			From ! {Name, ok, ack},
			loop(Name, F1, State);
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