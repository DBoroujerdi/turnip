-module(turnip_consumer).

-behaviour(gen_server).

%% API
-export([start_link/2,
         event/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("deps/amqp/include/amqp_client.hrl").

-define(SERVER, ?MODULE).

-record(state, {mod     :: atom(),
                tag     :: reference(),
                queue   :: binary(),
                channel :: pid()}).

%% todo: opens a new channel and reacts when channel dies? therefore,
%% turnip api should expose links errors.

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link(Queue, Callback) ->
    gen_server:start_link(?MODULE, [Queue, Callback], []).

event(Pid, Event) ->
    gen_server:cast(Pid, Event).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([Queue, Callback]) ->
    process_flag(trap_exit, true),
    self() ! init,
    {ok, #state{queue=Queue, mod=Callback}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(connection_up, State) ->
    self() ! init,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver'{delivery_tag = Tag}, {amqp_msg, _, Content}},
            #state{mod=Mod, channel=Channel} = State) ->
    case Mod:handle(Content) of
        ack ->
            ok = turnip:acknowledge(Channel, Tag),
            {noreply, State};
        _ ->
            {noreply, State}
    end;

%% handle_info({'EXIT', Pid, _}, #state{channel = Pid} = State) ->
%%     self() ! init,
%%     {noreply, State#state{tag=undefined, channel=undefined}};
handle_info(init, State) ->
    {ok, Channel} = turnip:open_channel(),
    %% true = link(Channel),
    ok = turnip_consumer_registry:register(),
    self() ! subscribe,
    {noreply, State#state{channel=Channel}};
handle_info(subscribe, #state{channel=Channel, queue=Queue} = State) ->
    {ok, Tag} = turnip:subscribe(Channel, Queue),
    {noreply, State#state{tag=Tag}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------
