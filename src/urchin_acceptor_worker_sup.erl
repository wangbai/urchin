-module(urchin_acceptor_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
        start_worker/1]).

%% supervisor callback
-export([init/1]).

%% macros
-define(SERVER, ?MODULE).

%% ------------------------------------
%% API
%% ------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker(ListenSocket) ->
    supervisor:start_child(?SERVER, [ListenSocket]).

%% ------------------------------------
%% supervisor callback
%% ------------------------------------

init([]) ->
    RestartStrategy = {simple_one_for_one, 1, 3600},
    AcceptorWorkerSpec = {urchin_acceptor_worker,
                 {urchin_acceptor_worker, start_link, []},
                 temporary,
                 brutal_kill, % Shutdown time
                 worker,
                 [urchin_acceptor_worker]},

    {ok, {RestartStrategy, [AcceptorWorkerSpec]}}.
