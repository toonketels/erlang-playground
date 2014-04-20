-module(musicians).
-behavior(gen_server).

-export([start_link/2,stop/1]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

% Role is passed as unique identifier for the musicican (there can be only one)
%
start_link(Role,Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
    gen_server:call(Role, stop).

% We pass the timeout as third item return value.
% 
init([Role,Skill]) ->
    process_flag(trap_exit, true),
    random:seed(now()),
    TimeToPlay = random:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),
    io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
    {ok, #state{name=Name, role=Role, skill=Skill}, TimeToPlay}.

code_change(_,_,_) -> todo.
handle_call(_,_,_) -> todo.
handle_cast(_,_) -> todo.
handle_info(_,_) -> todo.
terminate(_,_) -> todo.

pick_name() -> todo.
