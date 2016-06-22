-module(turnip_example_consumer).

-behaviour(turnip_consumer_behaviour).

-export([init/0,
         handle/2]).

%%------------------------------------------------------------------------------
%% callbacks
%%------------------------------------------------------------------------------

init() ->
    {ok, #{}}.

handle(Msg, State) ->
    io:format("~p MSG: ~p~n", [self(), Msg]),
    {ack, State}.
