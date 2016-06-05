
# Turnip

Turnip is a wrapper around the [RabbitMq](https://github.com/rabbitmq/rabbitmq-erlang-client) library, providing some useful extra features, such as,

- Broker connection management
- Simpler api
- Consumer pooling
- Consumer behaviour

## Usage

```erlang
ok = turnip:start(),

{ok, Channel} =  open:channel(),

ok = turnip:declare_exchange(Channel, <<"exchange_name">>),

ok = turnip:start_consumers(<<"my_queue">>, callback_module, 3),

ok = turnip:publish(Channel, <<"payload">>, <<"exchange_name">>, <<"*">>).
```

## todo: base documentation on peters readme docs

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
