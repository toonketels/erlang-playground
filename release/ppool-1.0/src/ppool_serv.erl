% Orchestrates the actual work in the pool and communication with outside
% world (pool is full...)
%
% Started by ppool_sup.
%
-module(ppool_serv).
-behaviour(gen_server).
-export([start/4,start_link/4,run/2,sync_queue/2,async_queue/2,stop/1]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

% Marco to specify the worker supervisor spec.
-define(SPEC(MFA),
        {worker_sup,
         {ppool_worker_sup, start_link, [MFA]},
         permanent,
         10000,
         supervisor,
         [ppool_worker_sup]}).

-record(state, {limit=0,             % Max allowed processes
                sup,                 % Pid of the ppool_worker_sup
                refs,
                queue=queue:new()}).

%
% Public interface
%

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

% Instructs the ppool_serv to run a task.
% Will be rejected when the pool is ful.
%
run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

% Instructs the ppool_serv to run a task,
% keeps the caller waiting until there is room.
%
% We set the timeout to infinity to prevent timing
% out before job completes.
%
sync_queue(Name, Args) ->
    gen_server:call(Name, {sync, Args}, infinity).

% Instructs ppool_serv to run a task as soon
% as possible, queuing it until there is room
%
async_queue(Name, Args) ->
    gen_server:cast(Name, {async, Args}).

stop(Name) ->
    gen_server:call(Name, stop).


%
% Implementation
%


% On startup, we want to initialize our state, and instruct our
% supervisor to create a new child supervisor to monitor the workers.
%
% We could do this by just doing `supervisor:start_child(Sup, ?SPEC(MFA)).`
% in the init/1 func. However this will cause a deadlock (our supervisor waits
% for the init function to return and we wait for the supervisor to create the
% child supervisor...)
%
% To step out of the deadlock, we send ourself a message to create the supervisor.
% We can only read it as soon as we return our init.
init({Limit, MFA, Sup}) ->
    % We ask our supervisor, to create a new one, to supervise
    % the worker processes.
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

% Not really upgrading...
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Handle the messages...
%
handle_call({run, Args}, _From, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};
handle_call({run, _Args}, _From, S=#state{limit=N}) when N =< 0 ->
    {reply, noalloc, S};

handle_call({sync, Args}, _From, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};
handle_call({sync, Args}, From, S=#state{limit=N, queue=Q}) when N =< 0 ->
    {noreply,S#state{queue=queue:in({From, Args},Q)}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

% Async messages...
%
handle_cast({async, Args}, S=#state{limit=N, sup=Sup, refs=R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {noreply, S#state{limit=N-1, refs=gb_sets:add(Ref, R)}};

handle_cast({async, Args}, S=#state{limit=N, queue=Q}) when N =< 0 ->
    {noreply,S#state{queue=queue:in(Args, Q)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

% We handle the message we send to ourself to create the supervisor.
%
handle_info({start_worker_supervisor, Sup, MFA}, S=#state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    {noreply, S#state{sup=Pid}};

% Whenever a worker goes down, notify and dequeue...
%
handle_info({'DOWN', Ref, process, _Pid, _}, S=#state{refs=Refs}) ->
    case gb_sets:is_element(Ref, Refs) of
        true ->
            handle_down_worker(Ref, S);
        false ->
            % We only care about our workers...
            {noreply, S}
    end;

handle_info(Message, S) ->
    io:format("Unknown message: ~p~n", [Message]),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.


% Whenever a worker goes down, we try to get a new one out of the queue.
% Depending on the kind (async, sync) we act differently.
%
% We pattern match on {{value ...}} so our code is only executed when the queue
% is not empty.
%
handle_down_worker(Ref, S=#state{limit=L, sup=Sup, refs=Refs}) ->
    case queue:out(S#state.queue) of
        % sync, we need to reply...
        {{value, {From, Args}}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            gen_server:reply(From, {ok, Pid}),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        % async, we dont need to reply
        {{value, Args}, Q} ->
            {ok, Pid} = supervisor:start_child(Sup, Args),
            NewRef = erlang:monitor(process, Pid),
            NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),
            {noreply, S#state{refs=NewRefs, queue=Q}};
        % queue is empty...
        {empty, _} ->
            {noreply, S#state{limit=L+1, refs=gb_sets:delete(Ref, Refs)}}
    end.
