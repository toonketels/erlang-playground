%%% The core of the app: the server in charge of tracking processes.
-module(regis_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/0, stop/0, register/2, unregister/1, whereis/1,
         get_names/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% Give a name to a process
register(Name, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

%% Remove the name from a process
unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%% Find the pid associated with a process
% This is save, so any process may call this code.
whereis(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name,Pid,_}] -> Pid;
        [] -> undefined
    end.
%% Find all the names currently registered.
get_names() ->
    MatchSpec = ets:fun2ms(fun({Name,_,_}) -> Name end),
    ets:select(?MODULE, MatchSpec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Our state is now stored in a ETS table with the name of our module.
% We create the table here.
init([]) ->
    ?MODULE = ets:new(regis_server, [set, named_table, protected]),
    {ok, ?MODULE}.

% Write to ETS regis table to register
% The state Tid is the table id (which is regis so reads can be done by other
% processes).
handle_call({register, Name, Pid}, _From, Tid) ->
    MatchSpec = ets:fun2ms(fun({N,P,_Ref}) when N==Name;P==Pid -> {N,P} end),
    case ets:select(Tid, MatchSpec) of
        [] -> % free to insert
            Ref = erlang:monitor(process, Pid),
            ets:insert(Tid, {Name, Pid, Ref}),
            {reply, ok, Tid};
        % Our matcher only returns the {N,P} and not the ref, so only two items
        % We might get a list with multiple matches, so we [{}|_]. this list is
        %      [{NameYouWant, SomePid},{SomeName,PidYouWant}]
        [{Name,_}|_] -> % name already taken
            {reply, {error, name_taken}, Tid};
        [{_,Pid}|_] -> % Pid already taken
            {reply, {error, already_named}, Tid}
    end;
handle_call({unregister, Name}, _From, Tid) ->
    case ets:lookup(Tid, Name) of
        [{Name, _Pid, Ref}] ->
            erlang:demonitor(Ref, [flush]),
            ets:delete(Tid, Name),
            {reply, ok, Tid};
        [] ->
            {reply, ok, Tid}
    end;
handle_call(stop, _From, Tid) ->
    % Drop the table...
    ets:delete(Tid),
    {stop, normal, ok, Tid};
handle_call(_Event, _From, State) ->
    {noreply, State}.
handle_cast(_Event, State) ->
    {noreply, State}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, Tid) ->
    ets:match_delete(Tid, {'_','_',Ref}),
    {noreply, Tid};
handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
