-module(redis_pool_sup).

-behaviour(supervisor).

-export([start_link/1, start_link/2]).

-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(PoolName) ->
    start_link(PoolName, []).

start_link(PoolName, Options) ->
    supervisor:start_link(
      {local, sup_name(PoolName)}, ?MODULE, [PoolName, Options]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([PoolName]) ->
    init([PoolName, pool_config_options(PoolName)]);
init([PoolName, PoolOptions]) ->
    {ok, {{one_for_all, 5, 5},
          [{pool, {redis_pool, start_link, [PoolName, PoolOptions]},
            permanent, brutal_kill, worker, [redis_pool]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sup_name(PoolName) ->
    list_to_atom("redis_pool_sup_" ++ atom_to_list(PoolName)).

pool_config_options(Name) ->
    case application:get_all_env(Name) of
        {ok, Options} -> Options;
        undefined -> []
    end.
