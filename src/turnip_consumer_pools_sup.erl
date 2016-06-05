-module(turnip_consumer_pools_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
         start/3]).

-define(SERVER, ?MODULE).

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start(Queue, Callback, Num) ->
    supervisor:start_child(?MODULE, [Queue, Callback, Num]).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, {{simple_one_for_one, 3, 60},
          [{?MODULE,
            {turnip_consumer_pool_sup, start_link, []},
            temporary, 1000, worker, [?MODULE]}
          ]}}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------
