-module(turnip_consumer_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link(Queue, Callback, Num) ->
    supervisor:start_link(?MODULE, [Queue, Callback, Num]).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([Queue, Callback, Num]) ->
    SupFlags = {one_for_one, 1, 5},

    {ok, {SupFlags, [child(Id, Queue, Callback) || Id <- lists:seq(1, Num)]}}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------

child(Id, Queue, Callback) ->
    turnip_sup:worker(name(Id, Queue),
                      turnip_consumer,
                      permanent,
                      [Queue, Callback]).

name(Id, Queue) ->
    IdBin = list_to_binary(integer_to_list(Id)),
    binary_to_atom(<<Queue/binary, "_", IdBin/binary>>, utf8).
