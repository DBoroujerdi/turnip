-module(turnip_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->

    %% read connection props from config (use env?)
    {ok, App} = application:get_application(),
    {ok, Config} = application:get_env(App, broker_config),

    %% todo: validate config

    turnip_sup:start_link(Config).

stop(_State) ->
    ok.
