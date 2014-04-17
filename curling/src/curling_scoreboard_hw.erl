-module(curling_scoreboard_hw).
-export([add_point/1, next_round/0, set_teams/2, reset_board/0]).

set_teams(TeamA, TeamB) ->
    io:format("Scorboard: Team ~s vs. Team ~s~n", [TeamA, TeamB]).

next_round() ->
    io:format("Scoreboard: round over~n").

add_point(Team) ->
    io:format("Scoreboard: increased score of Team ~s by 1~n", [Team]).

reset_board() ->
    io:format("Scoreboard: reset Teams and scores to 0~n").
