-module(turnip_consumer_registry).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register/0,
         send_event/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {consumers :: map()}).

%% todo: periodically check if channels are down and clear them out

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register() ->
    gen_server:cast(?SERVER, {register, self()}).

-spec send_event(atom()) -> ok.
send_event(Event) ->
    gen_server:cast(?SERVER, {event, Event}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{consumers = #{}}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({event, Event}, #state{consumers = Consumers} = State) ->
    lists:foreach(fun (Consumer) ->
                          turnip_consumer:event(Consumer, Event)
                  end, maps:keys(Consumers)),
    {noreply, State};
handle_cast({register, Consumer}, #state{consumers = Consumers} = State) ->
    case maps:is_key(Consumer, Consumers) of
        true ->
            %% already registered
            {noreply, State};
        false ->
            Mref = erlang:monitor(process, Consumer),
            {noreply, State#state{consumers=maps:put(Consumer, Mref, Consumers)}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, process, Pid, _}, #state{consumers = Consumers} = State) ->
    case maps:get(Pid, Consumers) of
        {MRef, Pid} ->
            {noreply, State#state{consumers=maps:remove(Pid, Consumers)}};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% private
%%------------------------------------------------------------------------------
