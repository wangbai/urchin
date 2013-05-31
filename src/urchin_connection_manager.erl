-module(urchin_connection_manager).
-behaviour(gen_server).

%% API
-export([start_link/0,
    start/0,
    stop/0,
    register_connection/2, 
    list_connections/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% gen_server state record
-record(state, {reg_table_ref}).

%% macros 
-define(SERVER, ?MODULE). 

%% ------------------------------------
%% API
%% ------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

register_connection(ConnName, ConnPid) when
    is_pid(ConnPid) -> 
    gen_server:call(?SERVER, {register, {ConnName, ConnPid}}).

list_connections() ->
    gen_server:call(?SERVER, {list}).

%% ------------------------------------
%% gen_server callbacks
%% ------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    % init registered connection table
    Opts = [set, protected, named_table],
    TableRef = ets:new(?SERVER, Opts),
    State = #state{reg_table_ref=TableRef},
    {ok, State}.

handle_call({register, {Uid, Pid}}, _From, State) ->
    TableRef = State#state.reg_table_ref,
    ets:insert(TableRef, {Uid, Pid}),
    Reply = {ok, {Uid, Pid}},
    {reply, Reply, State};
handle_call({list}, _From, State) ->
    TableRef = State#state.reg_table_ref,
    List = ets:tab2list(TableRef),
    Reply = {ok, List},
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, request_stop, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    TableRef = State#state.reg_table_ref,
    ets:delete(TableRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
