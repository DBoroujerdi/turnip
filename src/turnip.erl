-module(turnip).

-behaviour(application).

%% todo: learn and use erlang doc conventions

%% callbacks
-export([start/2,
         stop/1]).

%% Provide the additional functionality that turnip provides over and on
%% top of the underlying amqp library.
-export([start/0,
         start_consumer/2,
         start_consumers/3]).

%% These functions simply delegate to the underlying amqp library. They are
%% mainly used for those turnip features that require these functionality but
%% i also decided they could be useful to users of turnip as they would not
%% have to use both the amqp and turnip API in tandem, exposing their app to
%% just one API conventions.
-export([declare_exchange/1,
         declare_exchange/2,
         declare_exchange/3,
         delete_exchange/1,
         delete_exchange/2,
         declare_queue/0,
         declare_queue/1,
         delete_queue/1,
         bind/3,
         unbind/3,
         publish/2,
         publish/3,
         subscribe/1,
         subscribe/2,
         acknowledge/1]).


%%------------------------------------------------------------------------------
%% callbacks
%%------------------------------------------------------------------------------

start(_Type, _Args) ->

    %% read connection props from config (use env?)
    {ok, App} = application:get_application(),
    {ok, BrokerConfig} = application:get_env(App, broker_config),

    ok = validate(BrokerConfig),

    print_banner(),

    turnip_sup:start_link(BrokerConfig).

stop(_State) ->
    ok.

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start() ->
    application:ensure_all_started(?MODULE).

%% todo: not completely happy exposing this to the user
%% would like to find an elegant way to manage channels in the
%% library. Having to pass in channels to the Library may
%% cause unnecessary burden on the library user to have to manage them
%% themselves


%% -spec get_config() -> term().
%% get_config() ->
%%     application:get_env(?MODULE, config).


%% types
-type type() :: direct | fanout | headers.



-spec declare_exchange(binary()) -> ok.
declare_exchange(Name) ->
    turnip_channel_pool:execute(declare_exchange, [Name]).

-spec declare_exchange(pid(), list() | binary()) -> ok | {error | any()}.
declare_exchange(Channel, Name) ->
    declare_exchange(Channel, Name, direct).

-spec declare_exchange(pid(), Name, Type) -> ok | {error | any()} when
      Name :: binary() | string() | atom(),
      Type :: type().
declare_exchange(Channel, Name, Type) ->
    turnip_amqp:declare_exchange(Channel, Name, atom_to_binary(Type, utf8)).



-spec delete_exchange(binary()) -> ok.
delete_exchange(Name) ->
    turnip_channel_pool:execute(delete_exchange, [Name]).

-spec delete_exchange(pid(), binary()) -> ok.
delete_exchange(Channel, Name) ->
    turnip_amqp:delete_exchange(Channel, Name).



-spec declare_queue() -> ok.
declare_queue() ->
    turnip_channel_pool:execute(declare_queue).

-spec declare_queue(binary()) -> ok.
declare_queue(Name) ->
    turnip_channel_pool:execute(declare_queue, [Name]).

-spec delete_queue(binary()) -> ok.
delete_queue(Name) ->
    turnip_channel_pool:execute(delete_queue, [Name]).



-spec bind(binary(), binary(), binary()) -> ok.
bind(Queue, Exchange, RoutingKey) ->
    turnip_channel_pool:execute(bind, [Queue, Exchange, RoutingKey]).

-spec unbind(binary(), binary(), binary()) -> ok.
unbind(Queue, Exchange, RoutingKey) ->
    turnip_channel_pool:execute(unbind, [Queue, Exchange, RoutingKey]).



-spec publish(binary(), binary()) -> ok.
publish(Payload, RoutingKey) ->
    publish(Payload, RoutingKey, <<>>).

-spec publish(binary(), binary(), binary()) -> ok.
publish(Payload, RoutingKey, Exchange) ->
    turnip_channel_pool:execute(publish, [Payload, RoutingKey, Exchange]).



-spec subscribe(binary()) -> ok.
subscribe(Queue) ->
    subscribe(Queue, self()).

-spec subscribe(binary(), pid()) -> ok.
subscribe(Queue, Consumer) ->
    turnip_channel_pool:execute(subscribe, [Queue, Consumer]).



-spec acknowledge(reference()) -> ok.
acknowledge(Tag) ->
    turnip_channel_pool:execute(acknowledge, [Tag]).



-spec start_consumer(binary(), module()) -> ok.
start_consumer(Queue, Callback) ->
    start_consumers(Queue, Callback, 1).

-spec start_consumers(binary(), module(), number()) -> ok.
start_consumers(Queue, Callback, Num) ->
    turnip_consumer_pools_sup:start(Queue, Callback, Num).


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

validate(#{host := Host,
           port := Port,
           heartbeat := Heartbeat})
  when is_list(Host) and is_number(Port) and is_number(Heartbeat) ->
    ok.
