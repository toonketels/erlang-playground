-module(mafiapp).
-behaviour(application).
-include_lib("stdlib/include/ms_transform.hrl").
-export([install/1, add_friend/4,add_service/4,friend_by_name/1]).
-export([start/2, stop/1]).

-record(mafiapp_friends, {name, contact=[], info=[], expertise}).
-record(mafiapp_services, {from, to, date, description}).

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


%
% Application callbacks.
%

start(normal, []) ->
    % We start our app once mnesia has loaded all tables from disk/created tables
    mnesia:wait_for_tables([mafiapp_friends, mafiapp_services], 5000),
    mafiapp_sup:start_link().

stop(_) -> ok.
