-module(road).

-export([main/0, isShortest/2]).

main() ->
    File = "road.txt",
    {ok, Binary} = file:read_file(File),
    Roads = parse_map(Binary),
    Grouped = tregroup(Roads),
    % getShortestPath(Grouped).
    optimal_path(Grouped).


parse_map(Binary) when is_binary(Binary) ->
    parse_map(binary_to_list(Binary));
parse_map(String) when is_list(String) ->
    [list_to_integer(S) || S <- string:tokens(String, "\n")].

% Recursive regrouping.
%
% Note: we also take into account we might have to add
% the last 0 value.
% regroup([]) -> [];
% regroup([A, B]) -> [{A, B, 0}];
% regroup([A,B,X|T]) -> [{A,B,X} | regroup(T)].

% Tail recursive regrouping.
%
tregroup(L) -> tregroup(L, []).
tregroup([], Acc) -> lists:reverse(Acc);
tregroup([A,B], Acc) -> tregroup([], [{A,B,0}|Acc]);
tregroup([A,B,X | T], Acc) -> tregroup(T, [{A,B,X}|Acc]).

% Data data structure we're after:
%
% {length, [A, A, X, B, A]}
%
% For each step calculate what is the shortest to go
% to A or to B and we choose that route, and continue
getShortestPath(Paths) ->
    {Length, P} = lists:foldl(fun isShortest/2, {0, []}, Paths),
    {Length, lists:reverse(P)}.

isShortest({A,B,X}, {Length, []}) ->
    A1 = A,
    A2 = B + X,
    B1 = B,
    B2 = A + X,
    if  A1 < A2 -> ShortestA = {A1, [a]};
        true -> ShortestA = {A2, [b, x]}
    end,
    if  B1 < B2 -> ShortestB = {B1, [b]};
        true -> ShortestB = {B2, [a, x]}
    end,
    if  ShortestA < ShortestB -> Path = ShortestA;
        true -> Path = ShortestB
    end,
    {Path_length, Path_steps} = Path,
    {Length + Path_length, Path_steps};
% Now, we take the last selected into account
isShortest({A,B,X}, {Length, Previous = [a|_]}) ->
    if  A < X + B -> {Length + A, [a|Previous]};
        true -> {Length + X + B, [b,x | Previous]}
    end;
isShortest({A,B,X}, {Length, Previous = [b|_]}) ->
    if  B < X + A -> {Length + B, [b|Previous]};
        true -> {Length + X + A, [a,x | Previous]}
    end.




% Official implementation

optimal_path(L) ->
    {{DistA, PathA}, {DistB, PathB}} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, L),
    {Dist, Path} = erlang:min({DistA, PathA}, {DistB, PathB}),
    {Dist, lists:reverse(Path)}.


shortest_step({A,B,X}, {{DistA, PathA}, {DistB, PathB}}) ->
    OptA1 = {DistA + A, [{a,A}|PathA]},
    OptA2 = {DistB + B + X, [{x,X},{b,B}|PathB]},
    OptB1 = {DistB + B, [{b,B}|PathB]},
    OptB2 = {DistA + A + X, [{x,X}, {a,A}|PathA]},
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.



% getStreets() ->
%     {ok, Binary} = file:read_file("./road.txt"),
%     io:format("The file contents: ~p~n", [Binary]),
%     List =  binary_to_list(Binary),
%     io:format("The file contents: ~p~n", [List]),
%     List2 = string:tokens(List, "\n"),
%     io:format("The file contents: ~p~n", [List2]),
%     List3 = [list_to_integer(S) || S <- List2],
%     io:format("The file contents: ~p~n", [List3]).
