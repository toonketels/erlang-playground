-module(records).
-compile(export_all).
-include("record.hrl").

% To use records in the shell, we need  to"load" them via rr(module_name)

% Records... just glorified tupples...
% But won't break in patternmatching when a field is added (we can partially
% match on records, on tupples, we can't).

-record(user, {id, name, group, age}).
-record(robot, {name,
                 type=industrial,
                 hobbies,
                 details=[]}).

first_robot() ->
    #robot{name="R One", hobbies=["dancing"]}.

admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

adult_section(U) when U#user.age > 18 -> allowed;
adult_section(_) -> forbidden.

% Update a field in a record
repair(Bot) ->
    Details = Bot#robot.details,
    Bot2 = Bot#robot{details=["Repaired"|Details]},
    {repaired, Bot2}.

show_an_included() ->
    #included{name="An include"}.
