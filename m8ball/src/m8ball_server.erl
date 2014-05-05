% The actuall application.
-module(m8ball_server).
-behaviour(gen_server).
-export([start_link/0,stop/0,ask/1]).
-export([init/1,terminate/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2]).

%
% Pubic interface
%

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

% The m8ball does not really care about your question, he's got his answer ready
% no matter what. So we dont even bother passing the actual question around.
ask(_Question) ->
    gen_server:call({global,?MODULE}, question).




%
% Gen_server callbacks
%


% Initialize our random function. We could store the answers in the state but
% we keep them in the config file instead.
init([]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

% We pick a random answer from the config file...
% This is done on every call, so changes by application:set_env are instantly
% used. Its more efficient to only read on init.
handle_call(question, _From, State) ->
    {ok, Answers} = application:get_env(m8ball, answers),
    Answer = element(random:uniform(tuple_size(Answers)), Answers),
    {reply, Answer, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg,State) ->
    {noreply, State}.
