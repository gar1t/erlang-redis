-module(redis_pool_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(PoolName, Options) ->
    supervisor:start_link(?MODULE, [PoolName, Options]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([PoolName, PoolOptions]) ->
    ClientSup = client_sup_name(PoolName),
    {ok, {{one_for_all, 5, 5},
          [{client_sup, {redis_client_sup, start_link, [ClientSup]},
            permanent, brutal_kill, supervisor, [redis_client_sup]},
           {pool, {redis_pool, start_link, [PoolName, ClientSup, PoolOptions]},
            permanent, brutal_kill, worker, [redis_pool]}]}}.

client_sup_name(Pool) ->
    list_to_atom("redis_client_sup_" ++ atom_to_list(Pool)).
