-module(redis_client_sup).

-behaviour(supervisor).

-export([start_link/0, start_connection/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_connection(Options) ->
    supervisor:start_child(?SERVER, [Options]).

init([]) ->
    {ok, {{simple_one_for_one, 1, 1},
          [{connection, {redis_client, start_link, []},
            temporary, brutal_kill, worker, [redis_client]}]}}.
