
# Turnip

Turnip is a wrapper around the [RabbitMq](https://github.com/rabbitmq/rabbitmq-erlang-client) library, providing some useful extra features, such as,

- Broker connection management
- Simpler api
- Consumer pooling
- Consumer behaviour

## Basic Usage

```erlang
ok = turnip:start(),

{ok, Q} =  = turnip:declare_queue(),

ok = turnip:start_consumer(Q, turnip_example_consumer).

ok = turnip:publish(<<"Hello, World!">>, <<>>, Q).

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
