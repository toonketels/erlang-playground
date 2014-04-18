-module(curling_accumulator).
-behaviour(gen_event).
-export([init/1,terminate/2,code_change/3,handle_call/2,handle_info/2,handle_event/2]).

-record(state, {teams=orddict:new(), round=0}).

% Argument passed by calling gen_event:add_handler/3
%
init([]) -> {ok, #state{}}.

terminate(_Reason,_State) -> ok.

code_change(_OldVsn,State,_Extra) -> {ok, State}.

handle_info(_,State) -> {ok, State}.

handle_event({set_teams, TeamA, TeamB}, S=#state{teams=T}) ->
    Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
    {ok, S#state{teams=Teams}};
handle_event({add_points, Team, N}, S=#state{teams=T}) ->
    Teams = orddict:update_counter(Team, N, T),
    {ok, S#state{teams=Teams}};
handle_event(next_rount, S=#state{round=R}) ->
    {ok, S#state{round=R+1}};
handle_event(_Event, S) ->
    {ok, S}.


handle_call(game_data, S=#state{teams=T, round=R}) ->
    {ok, {orddict:to_list(T), {round, R}}, S};
handle_call(_, S) ->
    {ok, ok, S}.
