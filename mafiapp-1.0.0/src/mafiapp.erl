-module(mafiapp).
-behaviour(application).
-include_lib("stdlib/include/ms_transform.hrl").
-export([install/1, add_friend/4,add_service/4,friend_by_name/1,friend_by_expertise/1,debts/1]).
-export([find_enemy/1,add_enemy/2,enemy_killed/1]).
-export([start/2, stop/1]).

-record(mafiapp_friends, {name, contact=[], info=[], expertise}).
-record(mafiapp_services, {from, to, date, description}).
-record(mafiapp_enemies, {name, info=[]}).

% Specifies db schema to be installed on all the given nodes.
% Mnesia needs to be running to create the schema so we start/stop it.
install(Nodes) ->
    % Mnesia should not be running on a node to create schema,
    % For the tables, it should run on all the nodes.
    ok = mnesia:create_schema(Nodes),
    % On every node do: `application:start(mnesia)`
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(mafiapp_friends,
        [{attributes, record_info(fields, mafiapp_friends)},
         % Additional index on expertise, be default, only primary key is indexed
         {index, [#mafiapp_friends.expertise]},
         % Write to disk too
         {disc_copies, Nodes}]),
    mnesia:create_table(mafiapp_services,
        [{attributes, record_info(fields, mafiapp_services)},
         {index, [#mafiapp_services.to]},
         {disc_copies, Nodes},
         % See ETP for table types, almost all (but one) ETP types are supported.
         % We need bag because mulitple services can have the same from,to fields
         {type, bag}]),
    mnesia:create_table(mafiapp_enemies,
        [{attributes, record_info(fields, mafiapp_enemies)},
         {index, [#mafiapp_services.to]},
         {disc_copies, Nodes},
         % Keep this tables content private to this node
         % (each node has its own content)
         {local_content, true}]),
    % On every node do: `application:stop(mnesia)`
    rpc:multicall(Nodes, application, stop, [mnesia]).


add_friend(Name, Contact, Info, Expertise) ->
    F = fun() ->
        mnesia:write(#mafiapp_friends{name=Name,
                                      contact=Contact,
                                      info=Info,
                                      expertise=Expertise})
    end,
    mnesia:activity(transaction, F).

add_service(From,To,Date,Description) ->
    F = fun() ->
        % First check if From and To friend exists...
        case mnesia:read(mafiapp_friends, From) =:= [] orelse
             mnesia:read(mafiapp_friends, To) =:= [] of
            true ->
                {error, unknown_friend};
            false ->
                mnesia:write(#mafiapp_services{from=From,
                                               to=To,
                                               date=Date,
                                               description=Description})
        end
    end,
    mnesia:activity(transaction, F).


friend_by_name(Name) ->
    F = fun() ->
        case mnesia:read(mafiapp_friends, Name) of
            [#mafiapp_friends{contact=C, info=I, expertise=E}] ->
                {Name, C, I, E, find_services(Name)};
            [] ->
                undefined
        end
    end,
    mnesia:activity(transaction, F).


% Returns all the services the person was involed in (from, to)...
find_services(Name) ->
    Match = ets:fun2ms(
            fun(#mafiapp_services{from=From, to=To, date=D, description=Desc})
                when From =:= Name ->
                    {to, To, D, Desc};
               (#mafiapp_services{from=From, to=To, date=D, description=Desc})
                when To =:= Name ->
                    {from, From, D, Desc}
            end
    ),
    mnesia:select(mafiapp_services, Match).


friend_by_expertise(Expertise) ->
    Pattern = #mafiapp_friends{_='_', expertise=Expertise},
    F = fun() ->
        Res = mnesia:match_object(Pattern),
        % We "list comprehend" to return a tuple with as last item the services
        % find for that person.
        [{Name, C, I, Expertise, find_services(Name)} || #mafiapp_friends{name=Name, contact=C, info=I} <- Res]
    end,
    mnesia:activity(transaction, F).

% Generates a list with all the other people performed services to or received
% services from and the amount, negative means they owe us, positive means we owe
% them
debts(Name) ->
    % Get the record out of the db, we record the other ones name, -1 if they owe us,
    % 1 if we owe them.
    Match = ets:fun2ms(
        fun(#mafiapp_services{from=From,to=To}) when From =:= Name -> {To, -1};
           (#mafiapp_services{from=From,to=To}) when To =:= Name -> {From, 1}
        end),
    % Function to actually perform the match on the db
    F = fun() -> mnesia:select(mafiapp_services, Match) end,
    % Fold function aggregates the values of a list, it creates a dictionary
    % with the count of all we owe/they owe us per person
    FoldF = fun({Person, N}, Dict) ->
        dict:update(Person, fun(X) -> X + N end, N, Dict)
    end,
    % We create the dictionary via folding over the return value of executing
    % the transaction of the db (which returns a list)
    Dict = lists:foldl(FoldF,
                      dict:new(),
                      mnesia:activity(transaction, F)),
    % Converts the dictionary to a list and sorts it
    lists:sort([{V,K} || {K,V} <- dict:to_list(Dict)]).

find_enemy(Name) ->
    F = fun() ->
        mnesia:read(mafiapp_enemies, Name)
    end,
    % We dont just return the result of the transaction because we dont want to
    % return lists but just a tuple or undefined.
    case mnesia:activity(transaction, F) of
        [] -> undefined;
        [#mafiapp_enemies{name=N,info=I}] -> {N,I}
    end.

add_enemy(Name, Info) ->
    F = fun() ->
        mnesia:write(#mafiapp_enemies{name=Name,info=Info})
    end,
    mnesia:activity(transaction, F).

enemy_killed(Name) ->
    F = fun() ->
        mnesia:delete({mafiapp_enemies, Name})
    end,
    mnesia:activity(transaction, F).




%
% Application callbacks.
%

start(normal, []) ->
    % We start our app once mnesia has loaded all tables from disk/created tables
    % mnesia:wait_for_tables([mafiapp_friends, mafiapp_services], 5000),
    mafiapp_sup:start_link([mafiapp_friends, mafiapp_services, mafiapp_enemies]).

stop(_) -> ok.
