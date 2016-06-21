-module(turnip_consumer_behaviour).

-callback init(Args :: list(term())) -> {ok, any()} |
                                        {error, Reason :: string()}.

%% todo: should acks be expressed as a return of handle??
%% todo: should pass state map in on the handle

-callback handle(Message :: binary()) -> atom().
