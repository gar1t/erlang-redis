-module(dbservice).

-behaviour(gen_server).

-export([start/0]).

-export([start_link/0, set_value/2, get_value/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(RETRY_INTERVAL_SECONDS, 5).

-record(state, {db}).

%%%===================================================================
%%% Application convenience
%%%===================================================================

start() ->
    application:start(redis),
    application:start(dbservice).

%%%===================================================================
%%% Process start / init
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    erlang:send(self(), connect),
    {ok, #state{}}.

%%%===================================================================
%%% API (client context)
%%%===================================================================

set_value(Key, Value) ->
    handle_db_call(gen_server:call(?SERVER, {set, Key, Value}, infinity)).

get_value(Key) ->
    handle_db_call(gen_server:call(?SERVER, {get, Key}, infinity)).

handle_db_call({'$internal_error', Err}) -> error(Err);
handle_db_call(Result) -> Result.

%%%===================================================================
%%% gen_server event dispatch (server context)
%%%===================================================================

handle_call({set, Key, Value}, _From, State) ->
    handle_set(Key, Value, State);
handle_call({get, Key}, _From, State) ->
    handle_get(Key, State).

handle_info(connect, State) ->
    handle_connect(State).

%%%===================================================================
%%% Connection state management
%%%===================================================================

handle_connect(#state{db=undefined}=State) ->
    handle_redis_connect(redis:connect(), State).

handle_redis_connect({ok, Db}, State) ->
    handle_connected(Db, State);
handle_redis_connect({error, Err}, State) ->
    handle_connect_error(Err, State).

handle_connected(Db, State) ->
    link(Db),
    log_connected(),
    {noreply, State#state{db=Db}}.

log_connected() ->
    error_logger:info_msg("Connected to Redis server~n").

handle_connect_error(Err, State) ->    
    log_connect_error(Err),
    schedule_connect_retry(),
    {noreply, State}.

log_connect_error(Err) ->
    error_logger:error_msg("Error connecting to Redis server ~p~n", [Err]).

schedule_connect_retry() ->
    error_logger:info_msg(
      "Retrying Redis connection in ~p seconds~n", [?RETRY_INTERVAL_SECONDS]),
    erlang:send_after(?RETRY_INTERVAL_SECONDS * 1000, self(), connect).

%%%===================================================================
%%% DB operations
%%%===================================================================

handle_set(_, _, #state{db=undefined}=State) ->
    {reply, {'$internal_error', not_connected}, State};
handle_set(Key, Value, #state{db=Db}=State) ->
    {reply, redis:set(Db, Key, Value), State}.

handle_get(_, #state{db=undefined}=State) ->
    {reply, {'$internal_error', not_connected}, State};
handle_get(Key, #state{db=Db}=State) ->
    {reply, redis:get(Db, Key), State}.

%%%===================================================================
%%% gen_server boilerplate
%%%===================================================================

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
