-module(zoo).
-export([main/0]).

-type red_panda() :: bamboo | birds | eggs | berries.
-type squid() :: sperm_whale.
-type food(A) :: fun(() -> A).

-spec feeder(red_panda) -> food(red_panda());
            (squid) -> food(squid()).
feeder(red_panda) ->
    fun() ->
        element(random:uniform(4), {bamboo, birds, eggs, berries})
    end;
feeder(squid) ->
    fun() -> sperm_whale end.

-spec feed_red_panda(food(red_panda())) -> red_panda().
feed_red_panda(Generator) ->
    Food = Generator(),
    io:format("Feeding ~p to the panda~n", [Food]),
    Food.

-spec feed_squid(food(squid())) -> squid().
feed_squid(Generator) ->
    Food = Generator(),
    io:format("Throwing ~p in the squid\'s aquariom~n", [Food]),
    Food.


main() ->
    % Generate seed
    <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),

    FeederRP = feeder(red_panda),
    FeederSquid = feeder(squid),

    % Not right!!!
    feed_red_panda(FeederSquid),
    feed_squid(FeederRP).
