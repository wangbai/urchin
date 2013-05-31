-module(example).
-behaviour(gen_server).

%% API
-export([start_link/0,
    start/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% gen_server state record
-record(state, {count}).

%% macros 
-define(SERVER, ?MODULE). 

%% ------------------------------------
%% API
%% ------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------
%% gen_server callbacks
%% ------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    timer:sleep(10000),
    {ok, #state{count=0}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    Reply = {ok, Request},
    NewState=#state{ count = State#state.count+1 },
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    io:format("Cast:~p~n", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("Info:~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Terminate:~p~n", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
