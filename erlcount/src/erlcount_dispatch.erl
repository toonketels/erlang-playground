% Looks through the files and directories searching for .erl files.
%
% When found, schedules them to be analysed for the accurrance of specific
% words (see regexes in .app)

-module(erlcount_dispatch).
-behavior(gen_fsm).

-export([start_link/0,complete/4]).
-export([init/1,terminate/3,handle_event/3,handle_sync_event/4,
          handle_info/3,code_change/4]).
-export([dispatching/2,listening/2]).

-define(POOL, erlcount).

% To keep track of our statedata.
-record(data, {regex=[], refs=[]}).

%
% public interface methods.
%

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

% Whenever a worker indicates is done
complete(Pid,Regex,Ref,Count) ->
    gsm_fsm:send_all_state_event(Pid, Regex, Ref, Count).


%
% Fsm callbacks.
%


init([]) ->
    {ok, Re} = application:get_env(regex),
    {ok, Dir} = application:get_env(directory),
    {ok, MaxFiles} = application:get_env(max_files),
    ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
    case lists:all(fun valid_regex/1, Re) of
        true ->
            % Send ourself a message to start and setup our state.
            self() ! {start, Dir},
            {ok, dispatching, #data{regex=[{R, 0} || R <- Re]}};
        false ->
            {stop, invalid_regex}
    end.

terminate(_Reason,_State,_Data) ->
    ok.


% Global events...
%
% Jobs report back to use via global events since we can be in either dispatching,
% listening state.
%
handle_event({complete, Regex, Ref, Count}, State, Data=#data{regex=Re, refs=Refs}) ->
    {Regex, OldCount} = lists:keyfind(Regex, 1, Re),
    NewRe = list:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
    NewData = Data#data{regex=NewRe, refs=Refs--[Ref]},
    case State of
        dispatching ->
            {next_state, dispatching, NewData};
        listening ->
            % We call listening state manually since we might not receive any
            % messages anymore (see below).
            %
            % By calling listening we check if we're done (all jobs reported back)
            listening(done, NewData)
    end.


handle_sync_event(Event,_From,State,Data) ->
    io:format("Unexpected event ~p while in ~p state.", [Event, State]),
    {next_state, State, Data}.

% Whenever a msg is send to us via Pid ! Msg
%

handle_info({start, Dir}, StateName, Data) ->
    % We send ourself a message with the return value of find_erl...
    % It will be handled in dispatching as this is our state we're in.
    gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
    {next_state, StateName, Data}.

code_change(_OldVsn,State,Data,_Extra) ->
    {ok, State, Data}.


%
% Out states...
%


% We've found a file, and now want to:
%   - issue one job per Regex to count occurences in file
%   - continue scanning for files
%
dispatching({continue, File, Continuation}, Data=#data{regex=Re, refs=Refs}) ->
    % Cb fun to create a job (and ref) per regex and the refs
    % to the Refs we already have
    F = fun({Regex, _Count}, NewRefs) ->
        Ref = make_ref(),
        ppool:async_queue(?POOL, [self(), Ref, File, Regex]),
        [Ref|NewRefs]
    end,
    % Actually applying the func above, we fold the Regexes lists
    NewRefs = lists:foldl(F, Refs, Re),
    % Same trick again, send ourself a message with the result of Continuation(),
    % which is the callback function passed as the return value.
    gen_fsm:send_event(self(), Continuation()),
    % We're still dispatching...
    {next_state, dispatching, Data#data{refs=NewRefs}};

% When we're done, we move to the listening state...
%
dispatching(done, Data) ->
    % We dont do: {next_state, listening, Data}
    % but call the next state manually.
    %
    % We do so because moving to the next state will make us waiting
    % for messages to receive. If does will not come, we'll hang forever.
    % There is a chance, all jobs have already reported back so we will
    % not get any messages back.
    %
    % By calling listening() manually with our state, we can force it
    % to check if we're done.
    listening(done, Data).

% We only wait for results of the jobs to come in, all files are scheduled.
%
% We might already have all results (refs is empty).
%
listening(done, #data{regex=Re, refs=[]}) ->
    [io:format("Regex ~s has ~p results.~n", [R,C]) || {R,C} <- Re],
    {stop, normal, done};

% Some entries are still missing, we keep in the listening state.
%
% The job results self are send via global events since they can happen
% while in dispatching and listening.
%
listening(done, Data) ->
    {next_state, listening, Data}.

% Its valid by trying it out on an empty string
%
valid_regex(Aregex) ->
    try re:run("", Aregex) of
        _ -> true
    catch
        error:badarg -> false
    end.
