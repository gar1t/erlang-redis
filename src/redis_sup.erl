-module(redis_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1, 1},
          [{redis_client_sup, {redis_client_sup, start_link, []},
            permanent, brutal_kill, worker, [redis_client_sup]}]}}.
