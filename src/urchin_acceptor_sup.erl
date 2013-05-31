-module(urchin_acceptor_sup).
-behaviour(supervisor).

%% API
-export([start_link/1,
        start_acceptor_worker_sup/0,
        stop_acceptor_worker_sup/0]).

%% supervisor callback
-export([init/1]).

%% macros
-define(SERVER, ?MODULE).

%% ------------------------------------
%% API
%% ------------------------------------

start_link(Port) when is_integer(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

start_acceptor_worker_sup() ->
    Spec = {urchin_acceptor_worker_sup,
            {urchin_acceptor_worker_sup, start_link, []},
            temporary,
            brutal_kill, % Shutdown time
            supervisor,
            [urchin_acceptor_worker_sup]},
   
    supervisor:start_child(?SERVER, Spec).

stop_acceptor_worker_sup() ->
    supervisor:terminate_child(?SERVER, urchin_acceptor_worker_sup),
    supervisor:delete_child(?SERVER, urchin_acceptor_worker_sup).   

%% ------------------------------------
%% supervisor callback
%% ------------------------------------

init([Port]) ->
    RestartStrategy = {one_for_all, 1, 5},
    AcceptorSpec = {urchin_acceptor,
                    {urchin_acceptor, start_link, [Port]},
                    permanent,
                    brutal_kill, % Shutdown time
                    worker,
                    [urchin_acceptor]},

    {ok, {RestartStrategy, [AcceptorSpec]}}.
