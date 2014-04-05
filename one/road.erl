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


% Official implementation

optimal_path(L) ->
    {A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, L),
    io:format("A: ~p~n", [PathA]),
    io:format("B: ~p~n", [PathB]),
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



% getStreets() ->
%     {ok, Binary} = file:read_file("./road.txt"),
%     io:format("The file contents: ~p~n", [Binary]),
%     List =  binary_to_list(Binary),
%     io:format("The file contents: ~p~n", [List]),
%     List2 = string:tokens(List, "\n"),
%     io:format("The file contents: ~p~n", [List2]),
%     List3 = [list_to_integer(S) || S <- List2],
%     io:format("The file contents: ~p~n", [List3]).

% Flawed implementation below.
%
% Made the error in shortest path to thing
% X represeted the X before.
% Since this is no longer the case, our algo
% now keeps selecting the same path after the
% first decision.

% All below is not usefull!!!

% Data data structure we're after:
%
% {length, [A, A, X, B, A]}
%
% For each step calculate what is the shortest to go
% to A or to B and we choose that route, and continue
getShortestPath(Paths) ->
    {Length, P, _} = lists:foldl(fun isShortest/2, {0, []}, Paths),
    {Length, lists:reverse(P)}.

isShortest({A,B,X}, {Length, []}) ->
    A1 = A,
    A2 = B + X,
    B1 = B,
    B2 = A + X,
    if  A1 < A2 -> ShortestA = {A1, [{a,A}], a};
        true -> ShortestA = {A2, [{x,X}, {b,B}], a}
    end,
    if  B1 < B2 -> ShortestB = {B1, [{b,B}], b};
        true -> ShortestB = {B2, [{x,X}, {a,A}], b}
    end,
    if  ShortestA < ShortestB -> Path = ShortestA;
        true -> Path = ShortestB
    end,
    {Path_length, Path_steps, End} = Path,
    {Length + Path_length, Path_steps, End};
% Now, we take the last selected into account
isShortest({A,B,X}, {Length, Previous, a}) ->
    if  A < X + A -> {Length + A, [{a,A}|Previous], a};
        true -> {Length + X + A, [{x,X},{a,A} | Previous], b}
    end;
isShortest({A,B,X}, {Length, Previous, b}) ->
    if  B < X + B -> {Length + B, [{b,B}|Previous],b};
        true -> {Length + X + B, [{x,X},{b,B} | Previous], a}
    end.
