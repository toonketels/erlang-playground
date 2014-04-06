-module(kitchen).
-compile(export_all).

%
% Client api - startup and hides the protocol used (message format)
%

start() -> start(dict:new()).
start(Fridge_contents) -> spawn(?MODULE, refrigerator, [Fridge_contents]).

% Only wait for 300 millisec for a given message to prevent blocking forever.
%
% If the process to responds after the timeframe, the message is stored in
% the mailbox. If we run the store/take command again, the responding (new)
% message will be printed, not the one already in the mailbox.
%
% Do flush(). from shell to see the contents of the mailbox.
store(Pid, Item) -> store(Pid, Item, 1).
store(Pid, Item, Amount) ->
    Pid ! {self(), {store, {Item, Amount}}},
    receive
        {Pid, Message} -> Message
    after 3000 ->
        io:format("This is taking too long!~n"),
        timeout
    end.

take(Pid, Item) -> take(Pid, Item, 1).
take(Pid, Item, Amount) ->
    Pid ! {self(), {take, {Item, Amount}}},
    receive
        {Pid, Message} -> Message
    after 3000 ->
        io:format("This is taking too long~n"),
        timeout
    end.

%
% Server implementation
%

refrigerator(State) ->
    receive
        {Pid, {store, {Item, Amount}}} ->
            State2 = dict:update(Item, fun (Old) -> Old + Amount end, Amount, State),
            % timer:sleep(4000),
            Pid ! {self(), {ok, "Stored it"}},
            refrigerator(State2);
        {Pid, {take, {Item, Amount}}} ->
            case dict:find(Item, State) of
                {ok, Value} when Value >= Amount ->
                    State2 = dict:update(Item, fun (Old) -> Old - Amount end, State),
                    Pid ! {self(), {ok, "Took it"}},
                    refrigerator(State2);
                {ok, _} ->
                    Pid ! {self(), {error, "Not enough in fridge"}},
                    refrigerator(State);
                error ->
                    Pid ! {self(), {error, "Not in fridge"}},
                    refrigerator(State)
            end
    end.
