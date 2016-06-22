-module(test_consumer).

-behaviour(turnip_consumer_behaviour).

-export([init/0,
         handle/2]).

%%------------------------------------------------------------------------------
%% callbacks
%%------------------------------------------------------------------------------

init() ->
    {ok, #{}}.

handle(Msg, State) ->
    ok = msg_collector:collect(Msg),
    {ack, State}.
