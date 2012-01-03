-module(redis_client_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1, start_connection/2]).

-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).

start_connection(Sup, Options) ->
    supervisor:start_child(Sup, [Options]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 1},
          [{connection, {redis_client, start_link, []},
            temporary, brutal_kill, worker, [redis_client]}]}}.
