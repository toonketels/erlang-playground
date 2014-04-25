% Example worker.
% Will nag, by sending a message every x time until a deadline has reached.
-module(ppool_nagger).
-export([start_link/4,stop/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

%
% Public interface funcs...
%

start_link(Task, Delay, Max, SendTo) ->
    gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%
% gen_server callbacks

% We provide a Delay timeout which will cause the timeout message to be send
% to us as we're not recieving any calls.
%
init({Task, Delay, Max, SendTo}) ->
    {ok, {Task, Delay, Max, SendTo}, Delay}.

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call(_Msg, _From, S) ->
    {noreply, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

% Because we set ourself up to timeout...
handle_info(timeout, {Task, Delay, Max, SendTo}) ->
    SendTo ! {self(), Task},
    if  Max =:= infinity ->
            % keep on nagging when we were told to keep going forever...
            {noreply, {Task, Delay, Max, SendTo}, Delay};
        Max =< 1 ->
            % stop nagging if we reached our max number of times to nag
            {stop, normal, {Task, Delay, 0, SendTo}};
        Max > 1 ->
            % schedule a new nag...
            {noreply, {Task, Delay, Max-1, SendTo}, Delay}
    end.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Reason, _State) ->
    ok.
