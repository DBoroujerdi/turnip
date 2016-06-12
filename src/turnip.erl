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
-export([open_channel/0,
         close_channel/1,
         declare_exchange/2,
         declare_exchange/3,
         delete_exchange/2,
         declare_queue/1,
         declare_queue/2,
         delete_queue/2,
         bind/4,
         unbind/4,
         publish/4,
         publish/5,
         subscribe/2,
         subscribe/3,
         acknowledge/2]).

%%------------------------------------------------------------------------------
%% callbacks
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
%% public
%%------------------------------------------------------------------------------

start() ->
    application:ensure_all_started(?MODULE).

%% todo: not completely happy exposing this to the user
%% would like to find an elegant way to manage channels in the
%% library. Having to pass in channels to the Library may
%% cause unnecessary burden on the library user to have to manage them
%% themselves

%% todo: producer pools - how will they work??

-spec open_channel() -> {ok, pid()}.
open_channel() ->
    turnip_connection_mgr:open_channel().

-spec close_channel(pid()) -> ok | {error, any()}.
close_channel(_Channel) ->
    ok.



%% -spec get_config() -> term().
%% get_config() ->
%%     application:get_env(?MODULE, config).


%% types
-type type() :: direct | fanout | headers.


-spec declare_exchange(pid(), list() | binary()) -> ok | {error | any()}.
declare_exchange(Channel, Name) ->
    declare_exchange(Channel, Name, direct).

-spec declare_exchange(pid(), Name, Type) -> ok | {error | any()} when
      Name :: binary() | string() | atom(),
      Type :: type().
declare_exchange(Channel, Name, Type) ->
    turnip_amqp:declare_exchange(Channel, Name, atom_to_binary(Type, utf8)).

-spec delete_exchange(pid(), binary()) -> ok | {error | any()}.
delete_exchange(Channel, Name) ->
    turnip_amqp:delete_exchange(Channel, Name).




%% rabbit will auto-generate a name
-spec declare_queue(pid()) -> {ok, Name} when
      Name :: binary().
declare_queue(Channel) ->
    turnip_amqp:declare_queue(Channel).

-spec declare_queue(pid(), binary()) -> ok | {error | any()}.
declare_queue(Channel, Name) ->
    turnip_amqp:declare_queue(Channel, Name).

-spec delete_queue(pid(), binary()) -> ok | {error | any()}.
delete_queue(Channel, Name) ->
    turnip_amqp:delete_queue(Channel, Name).



-spec bind(pid(), binary(), binary(), binary()) -> ok.
bind(Channel, Queue, Exchange, RoutingKey) ->
    turnip_amqp:bind(Channel, Queue, Exchange, RoutingKey).

-spec unbind(pid(), binary(), binary(), binary()) -> ok.
unbind(_Channel, _Queue, _Exchange, _RoutingKey) ->
    ok.




%% todo: remove once consumers is done
%% should create a publisher per queue/message-type?
publish(Channel, Payload, Exchange, RoutingKey) ->
    publish(Channel, Payload, Exchange, RoutingKey, []).

-spec publish(pid(), binary(), binary(), binary(), list()) -> ok.
publish(Channel, Payload, Exchange, RoutingKey, _Props) ->
    turnip_amqp:publish(Channel, Payload, Exchange, RoutingKey).


subscribe(Channel, Queue) ->
    subscribe(Channel, Queue, self()).

subscribe(Channel, Queue, Consumer) ->
    turnip_amqp:subscribe(Channel, Queue, Consumer).


acknowledge(Channel, Tag) ->
    turnip_amqp:acknowledge(Channel, Tag).


start_consumer(Queue, Callback) ->
    start_consumers(Queue, Callback, 1).

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
