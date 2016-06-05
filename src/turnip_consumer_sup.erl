-module(turnip_consumer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    ConsumerPoolSup = turnip_sup:supervisor(turnip_consumer_pools_sup, permanent, []),
    ConsumerRegistry = turnip_sup:worker(turnip_consumer_registry, permanent, []),

    {ok, {{one_for_one, 1, 5}, [ConsumerPoolSup, ConsumerRegistry]}}.
