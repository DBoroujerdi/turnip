-module(turnip_channel_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1,
         execute/2,
         execute/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {channel :: pid()}).

%%------------------------------------------------------------------------------
%% public
%%------------------------------------------------------------------------------

start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

execute(Pid, JobFn) ->
    execute(Pid, JobFn, []).

execute(Pid, JobFn, Args) ->
    gen_server:call(Pid, {exec, JobFn, Args}).

%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------

init([]) ->
    self() ! init,
    {ok, #state{}}.

handle_call({exec, JobFn, Args}, _, #state{channel = Channel} = State)
  when Channel =:= undefined ->
    {reply, erlang:apply(turnip, JobFn, [Channel] ++ Args), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    {ok, Channel} = turnip:open_channel(),
    {noreply, State#state{channel=Channel}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
