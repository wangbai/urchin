-module(urchin_acceptor_worker).
-behaviour(gen_server).

%% API
-export([start_link/1,
    start/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% gen_server state record
-record(state, {listen_socket, accept_looper}).

%% ------------------------------------
%% API
%% ------------------------------------

start_link(ListenSocket) when is_port(ListenSocket) ->
    gen_server:start_link(?MODULE, [ListenSocket], []).

start(ListenSocket) when is_port(ListenSocket) ->
    gen_server:start(?MODULE, [ListenSocket], []).

%% ------------------------------------
%% gen_server callbacks
%% ------------------------------------

init([ListenSocket]) ->
    process_flag(trap_exit, true),
    Pid = spawn( fun() -> accept_loop(ListenSocket) end ),
    link(Pid),
    {ok, #state{listen_socket=ListenSocket, accept_looper=Pid}}.

handle_call(_Request, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, ExitReason}, State) ->
    if
        Pid == State#state.accept_looper ->
            {stop, ExitReason, State};
        true ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------
%% Private
%% ------------------------------------

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
        io:format("Info-------:~p~n", [Socket]),
        Pid = spawn(fun() -> receive X -> X end end),
        inet:setopts(Socket, [{active, true}]),
        gen_tcp:controlling_process(Socket, Pid),
        accept_loop(ListenSocket);
    {error, emfile} ->
        io:format("Error:Too many files opened~n"),
        accept_loop(ListenSocket);
    {error, system_limit} ->
        io:format("Error:Too many Ports opened~n"),
        accept_loop(ListenSocket);
    {error, _Err} ->
        io:format("Error:~p~n", [_Err]),
        exit(accept_socket_invalid)
    end.
