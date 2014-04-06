-module(kitchen).
-compile(export_all).

%
% Client api - startup and hides the protocol used (message format)
%

start() -> start(dict:new()).
start(Fridge_contents) -> spawn(?MODULE, refrigerator, [Fridge_contents]).

store(Pid, Item) -> store(Pid, Item, 1).
store(Pid, Item, Amount) -> Pid ! {self(), store, {Item, Amount}}.

take(Pid, Item) -> take(Pid, Item, 1).
take(Pid, Item, Amount) -> Pid ! {self(), take, {Item, Amount}}.

%
% Server implementation
%

refrigerator(State) ->
    receive
        {Pid, store, {Item, Amount}} ->
            State2 = dict:update(Item, fun (Old) -> Old + Amount end, Amount, State),
            Pid ! {ok, "Stored it"},
            refrigerator(State2);
        {Pid, store, Item} ->
            State2 = dict:update(Item, fun (Old) -> Old + 1 end, 1, State),
            Pid ! {ok, "Stored it"},
            refrigerator(State2);
        {Pid, take, {Item, Amount}} ->
            case dict:find(Item, State) of
                {ok, Value} when Value >= Amount ->
                    State2 = dict:update(Item, fun (Old) -> Old - Amount end, State),
                    Pid ! {ok, "Took it"},
                    refrigerator(State2);
                {ok, _} ->
                    Pid ! {error, "Not enough in fridge"},
                    refrigerator(State);
                error ->
                    Pid ! {error, "Not in fridge"},
                    refrigerator(State)
            end
    end.
