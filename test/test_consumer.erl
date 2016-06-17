-module(test_consumer).

-behaviour(turnip_consumer_behaviour).

-export([init/1,
         handle/1]).

%%------------------------------------------------------------------------------
%% callbacks
%%------------------------------------------------------------------------------

init(_Args) ->
    {ok, #{}}.

handle(Msg) ->
    ok = msg_collector:collect(Msg),
    ack.
