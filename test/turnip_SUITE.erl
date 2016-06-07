-module(turnip_SUITE).

-compile(export_all).

all() ->
    ct_common:all(?MODULE).

this_is_a_test(_Config) ->
    ct_common:doc("This is an example test case"),
    ok.
