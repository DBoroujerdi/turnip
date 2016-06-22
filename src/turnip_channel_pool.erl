-module(turnip_channel_pool).

-behaviour(supervisor).

%% API
-export([start_link/0,
         execute/1,
         execute/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(POOL_ID, turnip_channel_pool_server).


%%------------------------------------------------------------------------------
%% API functions
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

execute(Fun) ->
    execute(Fun, []).

execute(Fun, Args) ->
    Transaction =
        fun(Worker) ->
            turnip_channel_worker:execute(Worker, Fun, Args)
        end,

    try_execute(Transaction, Fun, Args, env:get_integer(turnip, channel_retry_count)).

try_execute(Transaction, Fun, Args, NumAttempts) ->
    case catch poolboy:transaction(?POOL_ID, Transaction) of
        ok -> ok;
        {ok, _} = Result -> Result;
        AnythingElse when NumAttempts > 1 ->
            error_logger:error_report(["Execute attempt failed",
                                       {result, AnythingElse},
                                       {'fun', Fun},
                                       {args, Args}]),
            try_execute(Transaction, Fun, Args, NumAttempts - 1);
        AnythingElse when NumAttempts =< 1 ->
            {error, [AnythingElse]}
    end.

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    PoolArgs = [{name, {local, ?POOL_ID}},
                {worker_module, turnip_channel_worker},
                {size, 10},
                {max_overflow, 0}],

    ChildSpec = poolboy:child_spec(?POOL_ID, PoolArgs, []),

    {ok, {{one_for_one, 5, 5}, [ChildSpec]}}.
