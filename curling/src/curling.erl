% Abstractiom module to handle interaction with the gen_event process.
%
% Note:this is not a behavior as this will specify the event handlers themselves
% and not the gen_event process in control of the handlers.
-module(curling).

-export([start_link/2,set_teams/3,add_points/3,next_round/1]).

start_link(TeamA, TeamB) ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, curling_scoreboard, []),
    set_teams(Pid, TeamA, TeamB),
    {ok, Pid}.

set_teams(Pid, TeamA, TeamB) ->
    gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

add_points(Pid, Team, N) ->
    gen_event:notify(Pid, {add_points, Team, N}).

next_round(Pid) ->
    gen_event:notify(Pid, next_round).