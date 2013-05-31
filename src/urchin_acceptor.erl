-module(urchin_acceptor).
-behaviour(gen_server).

%% API
-export([start_link/1,
    start/1,
    stop/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% gen_server state record
-record(state, {listen_socket, worker_sup, worker_num}).

%% macros 
-define(SERVER, ?MODULE). 
-define(MAX_BACKLOG, 1024). 
-define(MAX_WORKER_NUM, 1). 

%% ------------------------------------
%% API
%% ------------------------------------

start_link(Port) when 
        is_integer(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start(Port) when 
        is_integer(Port) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
    gen_server:call(?SERVER, stop).

%% ------------------------------------
%% gen_server callbacks
%% ------------------------------------

init([Port]) ->
    process_flag(trap_exit, true),

    Opts = [binary, 
            {packet, 0}, 
            {backlog, ?MAX_BACKLOG},
            {active, false}, 
            {reuseaddr,true}],

    case gen_tcp:listen(Port, Opts) of
    {ok, ListenSocket} -> 
        self() ! start_workers,
        {ok, #state{listen_socket = ListenSocket, 
                worker_num = ?MAX_WORKER_NUM}};
    Err ->
        {stop, Err}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_workers, State) ->
    % start acceptor worker sup
    {ok, WorkerSup} = urchin_acceptor_sup:start_acceptor_worker_sup(),
    link(WorkerSup),

    % start acceptor workers
    ListenSocket = State#state.listen_socket,
    WorkerNum = State#state.worker_num,
    urchin_util:forN(WorkerNum, fun create_worker/1, ListenSocket),  

    NewState = State#state{worker_sup=WorkerSup},
    {noreply, NewState};
handle_info({'EXIT', Pid, Reason}, State) ->
    if 
        Pid == State#state.worker_sup ->
            {stop, all_worker_exit, State};
        true -> 
            {stop, Reason, State}
    end;
handle_info({'DOWN', _, _, _, _}, State) ->
    {stop, worker_exit, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------
%% private
%% ------------------------------------

create_worker(ListenSocket) ->
    {ok, Pid} = urchin_acceptor_worker_sup:start_worker(ListenSocket),
    MonitorRef = monitor(process, Pid),
    {ok, Pid, MonitorRef}.
