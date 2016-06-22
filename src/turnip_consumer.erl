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
                state   :: any(),
                tag     :: reference(),
                queue   :: binary(),
                channel :: pid()}).

%% todo: opens a new channel and reacts when channel dies? therefore,
%% turnip api should expose links errors.

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link(Queue, Mod) ->
    gen_server:start_link(?MODULE, [Queue, Mod], []).

event(Pid, Event) ->
    gen_server:cast(Pid, Event).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([Queue, Mod]) ->
    process_flag(trap_exit, true),
    self() ! init,
    {ok, #state{queue=Queue, mod=Mod}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(connection_up, #state{mod=Mod} = State) ->
    self() ! init,
    {ok, ModState} = Mod:init(),
    %% todo: should call init on the callback module
    {noreply, State#state{state=ModState}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver'{delivery_tag = Tag}, {amqp_msg, _, Content}},
            #state{mod=Mod, state=ModState, channel=Channel} = State) ->
    case Mod:handle(Content, ModState) of
        {ack, NewModState} ->
            ok = turnip_amqp:acknowledge(Channel, Tag),
            {noreply, State#state{state=NewModState}};
        _ ->
            %% todo: how should we behave here?
            {noreply, State}
    end;

%% handle_info({'EXIT', Pid, _}, #state{channel = Pid} = State) ->
%%     self() ! init,
%%     {noreply, State#state{tag=undefined, channel=undefined}};
handle_info(init, State) ->
    {ok, Channel} = turnip_connection_mgr:open_channel(),
    %% true = link(Channel),
    ok = turnip_consumer_registry:register(),
    self() ! subscribe,
    {noreply, State#state{channel=Channel}};
handle_info(subscribe, #state{channel=Channel, queue=Queue} = State) ->
    {ok, Tag} = turnip_amqp:subscribe(Channel, Queue),
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
