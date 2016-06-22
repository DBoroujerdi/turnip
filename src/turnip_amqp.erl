-module(turnip_amqp).

-compile(export_all).

-include("deps/amqp/include/amqp_client.hrl").

%% this module exists to call the amqp library from - i don't want to have to
%% include ampq headers everywhere..

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

%% todo: this connect code should be looked at. in both bases, embedded or
%% otherwise, config will be the same, so i need a polymorphic function
%% from which to create a amqp_params record type.

%% todo: need a spec which defines the broker config. and where should this
%% spec live. realising now that i don't know the idiomatic way for spec
%% placement.

-spec connect(map()) -> {ok, pid()} | {error, any()}.
connect(#{host := embedded}) ->
    start(#'amqp_params_direct'{});
connect(#{host := Host, heartbeat := HeartBeat}) ->
    start(#'amqp_params_network'{host = Host, heartbeat = HeartBeat}).

start(AmqpParams) ->
    amqp_connection:start(AmqpParams).

-spec declare_exchange(pid(), binary(), binary()) -> ok.
declare_exchange(Channel, Name, Type) ->
    Declare = #'exchange.declare'{exchange = Name, type = Type},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    ok.

-spec delete_exchange(pid(), binary()) -> ok.
delete_exchange(Channel, Name) ->
    Delete = #'exchange.delete'{exchange = Name},
    #'exchange.delete_ok'{} = amqp_channel:call(Channel, Delete),
    ok.

-spec declare_queue(pid()) -> {ok, binary()}.
declare_queue(Channel) ->
    Declare = #'queue.declare'{},
    #'queue.declare_ok'{queue = Queue}  = amqp_channel:call(Channel, Declare),
    {ok, Queue}.

-spec declare_queue(pid(), binary()) -> ok.
declare_queue(Channel, Name) ->
    Declare = #'queue.declare'{queue = Name},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    ok.

-spec delete_queue(pid(), binary()) -> ok.
delete_queue(Channel, Name) ->
    Delete = #'queue.delete'{queue = Name},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete),
    ok.

-spec bind(pid(), binary(), binary(), binary()) -> ok.
bind(Channel, Queue, Exchange, RoutingKey) ->
    Binding = #'queue.bind'{queue       = Queue,
                            exchange    = Exchange,
                            routing_key = RoutingKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    ok.

-spec unbind(pid(), binary(), binary(), binary()) -> ok.
unbind(Channel, Queue, Exchange, RoutingKey) ->
    Binding = #'queue.unbind'{queue       = Queue,
                              exchange    = Exchange,
                              routing_key = RoutingKey},
    #'queue.unbind_ok'{} = amqp_channel:call(Channel, Binding),
    ok.

-spec open_channel(pid()) -> {ok, pid()} | {error, any()}.
open_channel(Connection) ->
    amqp_connection:open_channel(Connection).

-spec publish(pid(), binary(), binary(), binary()) -> ok.
publish(Channel, Payload, Key, Exchange) ->
    Publish = #'basic.publish'{exchange = Exchange, routing_key = Key},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}).

-spec subscribe(pid(), binary()) -> {ok, reference()}.
subscribe(Channel, Queue) ->
    subscribe(Channel, Queue, self()).

-spec subscribe(pid(), binary(), pid()) -> {ok, reference()}.
subscribe(Channel, Queue, Consumer) ->
    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = Tag} =
        amqp_channel:subscribe(Channel, Sub, Consumer),
    {ok, Tag}.

-spec acknowledge(pid(), reference()) -> ok.
acknowledge(Channel, Tag) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------
