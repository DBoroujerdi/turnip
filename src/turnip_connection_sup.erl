-module(turnip_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link(BrokerConfig) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [BrokerConfig]).


%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([BrokerConfig]) ->
    ConnectionMgr = turnip_sup:worker(turnip_connection_mgr, permanent, [BrokerConfig]),
    TurnipChannelPool = turnip_sup:supervisor(turnip_channel_pool, permanent, []),

    {ok, {{one_for_one, 5, 5}, [ConnectionMgr, TurnipChannelPool]}}.
