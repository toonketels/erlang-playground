% Run program do:
% erl -noshell -run road main road.txt
-module(road).

-export([main/1,main/0]).

% Starts the app...
%
main() -> main(["road.txt"]).
main([Filename]) ->
    {ok, Binary} = file:read_file(Filename),
    Map = parse_map(Binary),
    io:format("~p~n", [optimal_path(Map)]),
    erlang:halt().

% Parses the file content into our
% desired data structure.
%
parse_map(Binary) when is_binary(Binary) ->
    parse_map(binary_to_list(Binary));
parse_map(String) when is_list(String) ->
    Road_list = [list_to_integer(S) || S <- string:tokens(String, "\n")],
    regroup(Road_list).

% Tail recursive regrouping.
%
regroup(L) -> regroup(L, []).
regroup([], Acc) -> lists:reverse(Acc);
regroup([A,B], Acc) -> regroup([], [{A,B,0}|Acc]);
regroup([A,B,X | T], Acc) -> regroup(T, [{A,B,X}|Acc]).


% Folds the data structure to find the shortest path.
%
optimal_path(L) ->
    {A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, L),
    io:format("A: ~p~n", [A]),
    io:format("B: ~p~n", [B]),
    {Dist, Path} = if hd(element(2,A)) =/= {x,0} -> A;
                      hd(element(2,B)) =/= {x,0} -> B
                  end,
    {Dist, lists:reverse(Path)}.

% On a given point we can:
%   - go ahead
%   - go ahead and make a turn
%
% Since go ahead is always bigger, we take the next steps into account.
%
% Basically it goes like.
%   - the shortest rout to two alternatives
%   - the shortest route to next two alternatives from previous two alternatives
%   - ...
shortest_step({A,B,X}, {{DistA, PathA}, {DistB, PathB}}) ->
    OptA1 = {DistA + A, [{a,A}|PathA]},
    OptA2 = {DistB + B + X, [{x,X},{b,B}|PathB]},
    OptB1 = {DistB + B, [{b,B}|PathB]},
    OptB2 = {DistA + A + X, [{x,X}, {a,A}|PathA]},
    Z = {One,Two} = {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)},
    io:format("Step: ~n", []),
    io:format("A   : ~p~n", [One]),
    io:format("B   : ~p~n", [Two]),
    Z.
