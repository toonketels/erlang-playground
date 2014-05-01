% The actual worker.
%
% Does:
%   - opens files
%   - runs regex and counts matches
%   - reports back

-module(erlcount_counter).
-behaviour(gen_server).
-export([start_link/4]).
-export([code_change/3,init/1,terminate/2,handle_call/3,handle_cast/2,handle_info/2]).

-record(state, {dispatcher, ref, file, re}).

%
% Public api
%

start_link(DispatcherPid, Ref, File, Regex) ->
    gen_server:start_link(?MODULE, [DispatcherPid, Ref, File, Regex], []).


%
% Gen server callbacks
%

init([DispatcherPid, Ref, File, Regex]) ->
    self() ! start,
    {ok, #state{dispatcher=DispatcherPid,
                ref=Ref,
                file=File,
                re=Regex}}.

code_change(_OldVsn,State,_Extra) ->
    {ok, State}.

terminate(_Reason,_State) ->
    ok.

handle_call(_Msg,_From,State) ->
    {noreply, State}.

handle_cast(_Msg,State) ->
    {noreply, State}.

% Doing actual work
handle_info(start,State=#state{ref=Ref, re=Re}) ->
    {ok, Bin} = file:read_file(State#state.file),
    Count = erlcount_lib:regex_count(Re, Bin),
    erlcount_dispatch:complete(State#state.dispatcher,Re,Ref,Count),
    {stop, normal, State}.
