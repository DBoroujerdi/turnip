-module(msg_collector).

-behaviour(gen_server).

%% API
-export([start/0]).

%% behaviour
-export([collect/1,
         subscribe/0,
         receive_msg/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 4000).


%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

collect(Msg) ->
    gen_server:cast(?SERVER, {msg, Msg}).

subscribe() ->
    gen_server:cast(?SERVER, {subscribe, self()}).

receive_msg() ->
    receive
        {msg, Msg} ->
            {ok, Msg}
    after
        ?TIMEOUT  ->
            {error, timeout}
    end.


%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, #{subs => []}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({subscribe, Pid}, #{subs := Pids} = State) ->
    {noreply, State#{subs => [Pid|Pids]}};
handle_cast({msg, _} = Msg, #{subs := Pids} = State) ->
    _ = lists:foreach(fun (Pid) -> Pid ! Msg end, Pids),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------
