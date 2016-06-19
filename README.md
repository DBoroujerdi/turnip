
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

### Example Env

```erlang
{turnip [
    {host, "localhost"},
    {port, 12345},
    {exchanges, [
        {"exchange_1", direct, false},
        {"exchange_2", fanout, true}
    ]},
    {queues, [
        "queue_1",
        "queue_2"
    ]},
    {bindings, [
        {"tickets", "customers"}
    ]},
    {consumers, [
        {}
    ]}
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
