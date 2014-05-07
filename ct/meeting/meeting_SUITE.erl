-module(meeting_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([carla/1,mark/1,dog/1,all_same_owner/1]).

% All will only run the session group...
all() -> [{group, session}].

% Session group will run once, it will run the clients group and then
% `all_same_owner` test.
%
% Clients group will run the 3 tests in parallel 10 times for optimal
% race condition triggering.
groups() -> [{session, [], [{group, clients}, all_same_owner]},
             {clients, [parallel, {repeat, 10}], [carla, mark, dog]}].

% When session starts, init meeting module...
init_per_group(session, Config) ->
    meeting:start(),
    Config;
init_per_group(_, Config) ->
    Config.


end_per_group(session, _Config) ->
    meeting:stop();
end_per_group(_, _Config) ->
    ok.

carla(_Config) ->
    meeting:book_room(women),
    timer:sleep(10),
    meeting:rent_projector(women),
    timer:sleep(10),
    meeting:use_chairs(women).

mark(_Config) ->
    meeting:rent_projector(man),
    timer:sleep(10),
    meeting:use_chairs(man),
    timer:sleep(10),
    meeting:book_room(man).

dog(_Config) ->
    meeting:rent_projector(animals),
    timer:sleep(10),
    meeting:use_chairs(animals),
    timer:sleep(10),
    meeting:book_room(animals).

all_same_owner(_Config) ->
    % Will only succeed if Owner value is the same...
    [{_, Owner}, {_, Owner}, {_, Owner}] = meeting:get_all_bookings().
