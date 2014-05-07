-module(meeting).
-export([rent_projector/1,use_chairs/1,book_room/1,
          get_all_bookings/0,start/0,stop/0]).

-record(bookings, {projector, chairs, room}).

%
% Public interface
%

start() ->
    Pid = spawn(fun() -> loop(#bookings{}) end),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

rent_projector(Group) ->
    ?MODULE ! {projector, Group}.

use_chairs(Group) ->
    ?MODULE ! {chairs, Group}.

book_room(Group) ->
    ?MODULE ! {room, Group}.

get_all_bookings() ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, get_bookings},
    receive
        {Ref, Reply} ->
            Reply
    end.


%
% Registry
%

loop(B = #bookings{}) ->
    receive
        stop -> ok;
        {From, Ref, get_bookings} ->
            From ! {Ref, [{room, B#bookings.room},
                           {chairs, B#bookings.chairs},
                           {projector, B#bookings.projector}]},
            loop(B);
        {projector, Group} -> loop(B#bookings{projector=Group});
        {chairs, Group} -> loop(B#bookings{chairs=Group});
        {room, Group} -> loop(B#bookings{room=Group})
    end.
