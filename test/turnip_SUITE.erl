-module(turnip_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    ct_common:all(?MODULE).

%% todo: need to figure out how to start rabbitmq broker from the test -
%% it looks like rabbitmq does this by running erlang.mk goals. Will need to
%% then work out how to remotely connect to one of these nodes. should be able
%% to start these via rabbit common. Tho, if this takes too long I should
%% just concede that a docker mq should be running beforehand.
%%
%% a benefit from starting and stopping rabbit from the tests is that I can
%% exactly for those scenarios.

%% todo: rabbitmq tests - search for 'hazel' - how do the tests set this config?

%%------------------------------------------------------------------------------
%% setup/teardown
%%------------------------------------------------------------------------------

init_per_suite(Config) ->
    application:set_env(turnip, broker_config,
                        #{
                           host => "docker_host", %% todo: this should be configurable e.g make tests RABBIT_HOST=docker_host
                           port => 5671,
                           heartbeat => 5
                         }),

    application:ensure_all_started(turnip),

    wait(),

    Config.

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config).

%%------------------------------------------------------------------------------
%% tests
%%------------------------------------------------------------------------------

this_is_a_test(Config) ->
    ct_common:doc("This is an example test case"),

    Config.

open_channel_test(Config) ->
    ct_common:doc("Tests that Turnip can open a new channel"),

    {ok, _ChannelPid} = turnip:open_channel(),

    %% todo: is it idiomatic to assert with a test framework assertion
    %% or to just do it with pattern matching?

    Config.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

wait() ->
    receive
    after
       10000 ->
            true
    end.
