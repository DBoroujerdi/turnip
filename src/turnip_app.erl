-module(turnip_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start(_Type, _Args) ->

    %% read connection props from config (use env?)
    {ok, App} = application:get_application(),
    {ok, Config} = application:get_env(App, broker_config),

    %% todo: validate config

    print_banner(),

    turnip_sup:start_link(Config).

stop(_State) ->
    ok.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

print_banner() ->
    {ok, Product} = application:get_key(id),
    {ok, Version} = application:get_key(vsn),

    io:format("~n  ##  ##         \\/ "
              "~n  ##  ##        ####     ~s ~s."
              "~n  ##########   ######"
              "~n  ######  ##   ####"
              "~n  ##########   ##" ++
              "~n~n              Starting client..."
              "~n",
              [Product, Version]).
