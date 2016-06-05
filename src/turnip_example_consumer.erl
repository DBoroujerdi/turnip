-module(turnip_example_consumer).

-behaviour(turnip_consumer_behaviour).

-export([init/1,
         handle/1]).

%%------------------------------------------------------------------------------
%% callbacks
%%------------------------------------------------------------------------------

init(_Args) ->
    {ok, #{}}.

handle(Msg) ->
    io:format("~p MSG: ~p~n", [self(), Msg]),
    ack.
