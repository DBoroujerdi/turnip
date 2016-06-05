-module(turnip_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-export([supervisor/1,
         supervisor/2,
         supervisor/3,
         supervisor/4,
         %%
         worker/1,
         worker/2,
         worker/3,
         worker/4]).

start_link(BrokerConfig) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [BrokerConfig]).

init([BrokerConfig]) ->
    ConnectionMgr = worker(turnip_connection_mgr, permanent, [BrokerConfig]),
    SubscribersSup = supervisor(turnip_consumer_sup, permanent, []),

    {ok, {{one_for_one, 1, 5}, [ConnectionMgr, SubscribersSup]}}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

%% todo: convert to use maps

supervisor(Module) ->
    supervisor(Module, permanent).

supervisor(Module, Restart) ->
    supervisor(Module, Restart, []).

supervisor(Module, Restart, Args) ->
    supervisor(Module, Module, Restart, Args).

supervisor(Id, Module, Restart, Args) ->
    child_spec(Id, Module, Restart, infinity, supervisor, Args).

worker(Module) ->
    worker(Module, permanent).

worker(Module, Restart) ->
    worker(Module, Restart, []).

worker(Module, Restart, Args) ->
    worker(Module, Module, Restart, Args).

worker(Id, Module, Restart, Args) ->
    child_spec(Id, Module, Restart, 5000, worker, Args).

child_spec(Id, Module, Restart, Shutdown, Type, Args) ->
    {Id, {Module, start_link, Args}, Restart, Shutdown, Type, [Module]}.
