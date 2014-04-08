%%% Event server for clients to connect to.
%%%
%%% It stores event information, spawns timers and notifies clients when timers
%%% are done.
-module(evserv).
-export([init/0]).

-record(state, {events,
                clients}).

-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

%
%  Public api
%




%
% Internals
%

% Server initializer, should start the loop with a correct state.
%
% Both events and clients will be orddicts.
init() ->
    loop(#state{events=orddict:new(),
                clients=orddict:new()}).


loop(S = #state{}) ->
    receive

        % Client wants to subscibe.
        % We monitor to be notified when client disconnects (dies). When so
        % we can remove the client record.
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            ClientsUpdated = orddict:store(Ref, Client, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients=ClientsUpdated});

        % Client creates notification.
        % We store the events record and link with the newly spawned timer.
        %
        % Not sure, why we linked?
        {Pid, MsgRef, {add, Name, Description, Timeout}} ->
            case valid_datetime(Timeout) of
                true ->
                    EventPid = event:start_link(Name, Timeout),
                    Event = #event{name=Name,
                                   description=Description,
                                   pid=EventPid,
                                   timeout=Timeout},
                    EventsUpdated = orddict:store(Name, Event, S#state.events),
                    Pid ! {MsgRef, ok},
                    loop(S#state{events=EventsUpdated});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;

        % Client cancels an event...
        {Pid, MsgRef, {cancel, Name}} ->
            Events = case orddict:find(Name, S#state.events) of
                    {ok, Event} ->
                        event:cancel(Event#event.pid),
                        orddict:erase(Name, S#state.events);
                    error ->
                            S#state.events
                    end,
            Pid ! {MsgRef,  ok},
            loop(S#state{events=Events});

        % Send by timer when it completes
        {done, Name} ->
            % notify subscribers, remove the
            Events = case orddict:find(Name, S#state.events) of
                        {ok, Event} ->
                            notify_all_clients(S#state.clients, Event),
                            orddict:erase(Name, S#state.events);
                        error ->
                            S#state.events
                    end,
            loop(S#state{events=Events});

        shutdown -> exit(shutdown);

        % We monitor the clients connecting to us so we get this
        % message whenever such a  client dies...
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=orddict:erase(Ref, S#state.clients)});

        % A new loop with absolute path to reload the code...
        code_change -> ?MODULE:loop(S);

        % {done, Name} -> todo;
        % shutdown -> todo;
        % {'DOWN', Ref, process, _Pid, _Reason} -> todo;
        % code_change -> todo;
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.


notify_all_clients([], _) -> ok;
notify_all_clients([ClientPid|T], Event) ->
    ClientPid ! {done, Event#event.name, Event#event.description},
    notify_all_clients(T, Event).



% Time validation...
%
valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause ->
            false
    end;
valid_datetime(_) -> false.

valid_time({H,Min,S}) -> valid_time(H, Min, S).
valid_time(H, M, S) when H >= 0, H < 24,
                         M >= 0, M < 60,
                         s >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
