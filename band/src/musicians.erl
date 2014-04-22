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
    % So we know when the parent shuts down...
    process_flag(trap_exit, true),
    random:seed(now()),
    TimeToPlay = random:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),            % Converts atom to string
    io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
    {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

code_change(_OldVsn,S,_Extra) ->
    {ok, S}.

handle_call(stop,_From,S=#state{}) ->
    {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
    {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
    {noreply, S, ?DELAY}.

% Whenever the server times out, we play a note.
% Bad musicions plays sometimes a bad note which will
% make it crash.
%
handle_info(timeout, S=#state{name=N, skill=good}) ->
    io:format("~p produced sound~n", [N]),
    {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=N, skill=bad}) ->
    case random:uniform(5) of
        1 ->
            io:format("~p played a bad note!~n", [N]),
            {stop, bad_note, S};
        _ ->
            io:format("~p produced sound~n", [N]),
            {noreply, S, ?DELAY}
    end;
handle_info(_message, S) ->
    {noreply, S, ?DELAY}.

% stop/1 is called
terminate(normal,S) ->
    io:format("~p left the room(~p)~n", [S#state.name, S#state.role]);
% {stop, bad_note, S} message has been returned (which crashes the process)
terminate(bad_note, S) ->
    io:format("~p sucks, kick him out of the band (~p)~n", [S#state.name, S#state.role]);
% Supervisors stops the process
terminate(shutdown, S) ->
    io:format("Manager fires the whole band, ~s goes back to play in the subway~n", [S#state.name]);
% All other reasons...
terminate(Reason, S) ->
    io:format("~s has been kicked out (~s) for reason ~p~n", [S#state.name, S#state.role, Reason]).

pick_name() ->
    lists:nth(random:uniform(10), firstnames()),
    lists:nth(random:uniform(10), lastnames()).

firstnames() ->
    ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
     "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

lastnames() ->
    ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
     "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].
