
# Turnip

Turnip is a wrapper around the [RabbitMq](https://github.com/rabbitmq/rabbitmq-erlang-client) library, providing some useful extra features, such as,

- Reconnection if connection is lost
- Consumer pooling with reconnecting channels when channel or connection is lost

## Usage

### Starting
```erlang
ok = turnip:start(),
```

### Basic publishing

```erlang
{ok, Q} =  = turnip:declare_queue(),

ok = turnip:start_consumer(Q, turnip_example_consumer).

<<<<<<< HEAD
ok = turnip:publish(Payload = <<"Hello, World!">>, RoutingKey = Q).

%% <0.629.0> MSG: <<"Hello, World!">>
```

### Routing Usage

```erlang
Exchange = <<"my_exchange">>,

ok = turnip:declare_exchange(Exchange),

{ok, Q} = turnip:declare_queue(),

%% this will start 5 processes all consuming from the same Q, each with
%% their own channel
{ok, _} = turnip:start_consumers(Q, turnip_example_consumer, _NumConsumers = 5),

ok = turnip:bind(Q, Exchange, <<"abcd.*">>),

ok = turnip:publish(_Payload = <<"Hello, World!">>,
                    _RoutingKey = <<"abcd.1234">>,
                    _Exchange = Exchange).

%% <0.629.0> MSG: <<"Hello, World!">>
```

## Examples

[Example consumer](src/turnip_example_consumer.erl), implementing the turnip_consumer_behaviour.

### Example Env

```erlang
{turnip, [{broker_config, #{
              host => "docker_host",
              port => 5671,
              heartbeat => 5}}
          ]}
```


### Example Config

```erlang
```


### Example Consumer

```erlang
```



## Authors

## Liscense
