-module(trade_fsm).
-behavior(gen_fsm).

%
% Do differentiate user-to-fsm and fsm-to-fsm communication
%


-record(state, {name="",
                other,
                ownitems=[],
                otheritems=[],
                monitor,
                from}).

% Public API
-export([start/1,start_link/1,trade/2,accept_trade/1,make_offer/2,retract_offer/2,ready/1,cancel/1]).
% gen_fsm callbacks
-export([code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,init/1,terminate/3]).
% custom state names
-export([idle/2,idle/3,idle_wait/2,idle_wait/3,negotiate/2,negotiate/3,wait/2,ready/2,ready/3]).


%
% Public API
%
% Me (a person) and my fsm
%


start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

% Initiate a trade sessions. Returns when the other accepts/refuses.
%
% OwnPid: us instruction our tade_fsm to initialize a trade with the other.
%
trade(OwnPid,OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid},3000).

% Instruct our fsm to accept to trade
%
accept_trade(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).

% Instruct our fsm to send an offer
%
make_offer(OwnPid,Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

% Instruct our fsm to cancel offer.
%
retract_offer(OwnPid,Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

% Notify other we are ready for trading...
% We will wait until other is ready too...
%
ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

% Cancel transaction
%
% This is send as a global event (no mother what the fsm state is)
%
cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).



%
% fsm-to-fsm communication
%
% Note: this is intra module so does not have to be exported
%


% fsm asks other fsm to start trading
%
ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

% fsn accepts trading session other fgen_fsm
%
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).


am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).




%
% gen_fsm callbacks
%

init(Name) ->
    {ok, idle, #state{name=Name}}.

terminate(normal, ready, State) ->
    notice(State, "terminating", []);
terminate(_Reason, _StateName, _State) ->
    ok.

% We're not updating the code for the moment...
%
code_change(_OldVsn, StateName, StateData, _Extra) ->
     {ok, StateName, StateData}.


% To handle global events, those send via gen_fsm:send_all_state_event/2
%

% Whenever the other player wants to cancel...
% We just stop (exit the fsm)
%
handle_event(cancel, _StateName, State) ->
    notice(State, "other player wants to cancel", []),
    {stop, other_cancelled, State};
handle_event(Event, StateName, State) ->
    unexpected(Event, StateName),
    {next_state, StateName, State}.

% We want to cancel...
%
handle_sync_event(cancel, _From, _StateName, State) ->
    notify_cancel(State#state.other),
    notice(State, "cancelling trade, sending cancel event", []),
    {stop, cancelled, ok, State};
% We let the caller crash by not replying...
handle_sync_event(Event, _From, StateName, State) ->
    unexpected(Event, StateName),
    {next_state, StateName, State}.


% When the other process goes down...
%
handle_info({'DOWN', Ref, process, Pid, Reason}, _StateName, State=#state{other=Pid, monitor=Ref}) ->
    notice(State, "other side dead", []),
    {stop, {other_down, Reason}, State};
handle_info(Event, StateName, State) ->
    unexpected(Event, StateName),
    {next_state, StateName, State}.



%
% Custom state names
%


% Aync messages in idle state...
% Its the other fsm as our user only communicates synchronously.
%
% @see: http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2
idle({ask_negotiate, OtherPid},S=#state{}) ->
    Ref=monitor(process, OtherPid),
    notice(S, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
idle(Event, S) ->
    unexpected(Event, idle),
    {next_state, idle, S}.

% Our client asks his fsm to negotiate a trade...
%
% Note: each 'client' runs in its own process and their fsm's too,
%       so From Pid is not the same as fsm self().
idle({negotiate, OtherPid},From,S=#state{}) ->
    Ref=monitor(process, OtherPid),
    ask_negotiate(OtherPid, self()),
    notice(S, "asking user ~p for a trade", [OtherPid]),
    {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, S) ->
    unexpected(Event, idle),
    {next_state, idle, S}.

% We wait for other client to accept negotiating or asking to negotiate (race cond).
%
% Here we match the OtherPid from the state to ensure its the same client we asked
% to start negotiating.
idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
    % fsm replies to sync_send_event from its client
    gen_fsm:reply(S#state.from, ok),
    % we dont have to notify the other fsm we accept because its wired the same
    % way our fsm works, it will also just move to negotiate state.
    notice(S, "Starting negotiation", []),
    {next_state, negotiate, S};
% The other accepts...
idle_wait({accept_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
    gen_fsm:reply(S#state.from, ok),
    notice(S, "Starting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, S) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, S}.

% Ouf fsm client is accepting the trade...
%
idle_wait(accept_negotiate, _From, S=#state{}) ->
    % Inform other fsm we want to trade.
    accept_negotiate(S#state.other, self()),
    notice(S, "accepting negotiation", []),
    {next_state, negotiate, S};
idle_wait(Event, _From, S) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, S}.

% We make and offer...
negotiate({make_offer, Item},S=#state{ownitems=OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=add(Item,OwnItems)}};
% We retract our offer...
negotiate({retract_offer, Item},S=#state{ownitems=OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "retracting ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=remove(Item,OwnItems)}};
% Other party makes an offer...
negotiate({do_offer, Item},S=#state{otheritems=OtherItems}) ->
    notice(S, "other player offered ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item,OtherItems)}};
% Other party cancels his offer.
negotiate({undo_offer, Item},S=#state{otheritems=OtherItems}) ->
    notice(S, "other player retracted ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item,OtherItems)}};
% Other party asks if we're ready, since we're not in the ready state,
% we reply no.
negotiate(are_you_ready,S=#state{other=OtherPid}) ->
    io:format("Other user ready to trade~n"),
    notice(S, "Other user ready to transfer goods:~n"
              "You get ~p. The other side gets ~p~n", [S#state.otheritems, S#state.ownitems]),
    not_yet(OtherPid),
    {next_state, negotiate, S};
negotiate(Event,S) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, S}.

% User informs his fsm he's ready to trade.
negotiate(ready,From,S=#state{other=OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "asking if ready, waiting", []),
    {next_state, wait, S#state{from=From}};
negotiate(Event,_From,S) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, S}.

% We're waiting for the other party to confirm he's ready...
%
% Since our client sends us a synchronous message to set the fsm
% in the wait state, we have to reply to our client now
%
wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item,OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side retracting ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item,OtherItems)}};
% Race condition again...
%
% We reply the other fsm
wait(are_you_ready, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if ready. I am, waiting for same reply", []),
    {next_state, wait, S};
wait(not_yet, S=#state{}) ->
    notice(S, "other party is not yet ready, waiting", []),
    {next_state, wait, S};
wait('ready!', S=#state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S, "other is ready, lets get ready", []),
    {next_state, ready, S};
wait(Event, S) ->
    unexpected(Event, wait),
    {next_state, wait, S}.


% Ready for 2 phase commit-ish
%
ready(ack, S=#state{}) ->
    case priority(self(), S#state.other) of
        true ->
            % We initialize synchronous communication
            try
                notice(S, "asking for commit", []),
                ready_commit = ask_commit(S#state.other),
                notice(S, "ordering commit", []),
                ok = do_commit(S#state.other),
                notice(S, "commiting...", []),
                commit(S),
                {stop, normal, ok, S}
            catch Class:reason ->
                % Abort, ask_commit or do_commit failed
                notice(S, "commit failed", []),
                {stop, {Class, reason}, S}
            end;
        false ->
            % We waith for synchronous message
            {next_state, ready, S}
        end;
ready(Event, S) ->
    unexpected(Event, ready),
    {next_state, ready, S}.

% Actuall commit is synchronous
%
ready(ask_commit, _From, S) ->
    notice(S, "being asked to commit", []),
    % strange syntax...
    {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
    notice(S, "commiting...", []),
    commit(S),
    {stop, normal, ok, S};
ready(Event, _From, S) ->
    unexpected(Event, ready),
    {next_state, ready, S}.



%
% Utility funcs.
%

% When the fsm wants to notify his user...
%
notice(#state{name=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).

% Log unexpected messages.
%
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

% We want to easily swap out the way we store items traded.
%
add(Item, Items) ->
    [Item | Items].

remove(Item, Items) ->
    Items -- [Item].

% To elect one or fhe fsm's to initiate synchronous calls (prevent deadlocking)
%
priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.


commit(S=#state{}) ->
    io:format("Transaction completed for ~s."
              "Items sent are:~n~p,~n received are:~n~p.~n"
              "This operation should have some atomic save "
              "in a database.~n",
              [S#state.name, S#state.ownitems, S#state.otheritems]).
