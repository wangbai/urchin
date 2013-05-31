-module(urchin_config).
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
-record(state, {config_data, role}).

%% macros 
-define(SERVER, ?MODULE). 

%% ------------------------------------
%% API
%% ------------------------------------

start_link(IsMaster) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [IsMaster], []).

start(IsMaster) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [IsMaster], []).

stop() ->
    gen_server:call(?SERVER, stop).

%% ------------------------------------
%% gen_server callbacks
%% ------------------------------------

init([IsMaster]) ->
    process_flag(trap_exit, true),

    {ok, #state{config_data = [],
            role = IsMaster}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
