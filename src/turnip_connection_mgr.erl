-module(turnip_connection_mgr).

-behaviour(gen_server).

%% API
-export([start_link/1,
         open_channel/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {con :: pid(), config :: turnip:broker_cfg()}).


%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link(BrokerConfig) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [BrokerConfig], []).

open_channel() ->
    gen_server:call(?SERVER, open_channel).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([BrokerConfig]) ->
    process_flag(trap_exit, true),
    self() ! connect,
    {ok, #state{config = BrokerConfig}}.

handle_call(open_channel, _, #state{con=undefined} = State) ->
    {reply, {error, no_connection}, State};
handle_call(open_channel, _, #state{con=Connection} = State) ->
    {reply, turnip_amqp:open_channel(Connection), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reconnect, State) ->
    %% todo: externalize send after prop
    erlang:send_after(2000, self(), connect),
    {noreply, State#state{con=undefined}};
handle_info(connect, #state{config = BrokerConfig} = State) ->
    case turnip_amqp:connect(BrokerConfig) of
        {ok, Connection} ->
            true = link(Connection),
            ok = turnip_consumer_registry:send_event(connection_up),
            {noreply, State#state{con=Connection}};
        _ ->
            self() ! reconnect,
            {noreply, State}
    end;
handle_info({'EXIT', _FromPid, _Reason}, _State) ->
    self() ! reconnect,
    {noreply, #state{}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------
