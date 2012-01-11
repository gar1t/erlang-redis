-module(redis_pool).

-behaviour(gen_server).

-export([start_link/2, start_link/3, acquire/1, acquire/2, release/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {client_sup, all, free, max, waiters, connect, db}).

-define(DEFAULT_RETRY_DELAY, 5000).
-define(DEFAULT_SIZE, 1).
-define(DEFAULT_CONNECT, []).
-define(DEFAULT_DATABASE, undefined).
-define(DEFAULT_ACQUIRE_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ClientSup, Options) ->
    gen_server:start_link(?MODULE, [ClientSup, Options], []).

start_link(PoolName, ClientSup, Options) ->
    gen_server:start_link(
      {local, server_name(PoolName)}, ?MODULE, [ClientSup, Options], []).

acquire(Pool) ->
    gen_server:call(
      server_name(Pool), {acquire, ?DEFAULT_ACQUIRE_TIMEOUT}, infinity).

acquire(Pool, Timeout) ->
    gen_server:call(server_name(Pool), {acquire, Timeout}, infinity).

release(Pool, Connection) ->
    gen_server:cast(server_name(Pool), {release, Connection}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ClientSup, Options]) ->
    State = init_state(ClientSup, Options),
    schedule_connect(State),
    {ok, State}.

handle_call({acquire, Timeout}, From, State) ->
    schedule_dispatch(),
    {noreply, add_waiter(From, Timeout, State)}.

handle_cast({connect_result, {ok, Connection}}, State0) ->
    State = add_connection(Connection, State0),
    select_db(Connection, State),
    schedule_next_connect(State),
    {noreply, State};
handle_cast({connect_result, {error, Err}}, State) ->
    error_logger:error_msg("Error connecting to Reds server: ~p~n", [Err]),
    schedule_connect_retry(State),
    {noreply, State};
handle_cast({release, Connection}, State) ->
    {noreply, release_connection(Connection, State)}.

handle_info(connect, State) ->
    {noreply, try_connect(State)};
handle_info(dispatch, State) ->
    {noreply, try_dispatch(State)};
handle_info({acquire_timeout, Waiter}, State) ->
    gen_server:reply(Waiter, timeout),
    {noreply, remove_waiter(Waiter, State)};
handle_info({'DOWN', _Ref, process, Connection, _Info}, State) ->
    schedule_next_connect(State),
    {noreply, remove_connection(Connection, State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

server_name(Pool) ->
    list_to_atom("redis_pool_" ++ atom_to_list(Pool)).

init_state(ClientSup, Options) ->
    Max = proplists:get_value(size, Options, ?DEFAULT_SIZE),
    ConnectOptions = proplists:get_value(
                       connect, Options, ?DEFAULT_CONNECT),
    Db = proplists:get_value(database, Options, ?DEFAULT_DATABASE),
    #state{client_sup=ClientSup,
           max=Max,
           connect=ConnectOptions,
           db=Db,
           all=[],
           free=queue:new(),
           waiters=queue:new()}.

try_connect(#state{client_sup=ClientSup, connect=Options}=State) ->
    error_logger:info_msg("Connecting to Redis server: ~p~n", [Options]),
    Pool = self(),
    spawn_link(fun() -> try_connect(ClientSup, Options, Pool) end),
    State.

try_connect(Sup, ClientOptions, Pool) ->
    try redis_client_sup:start_connection(Sup, ClientOptions) of
        Result -> notify_connect(Pool, Result)
    catch
        T:E ->
            ST = erlang:get_stacktrace(),
            notify_connect(Pool, {error, {T, E, ST}})
    end.

try_dispatch(State0) ->
    case next_waiter_and_connection(State0) of
        {{Waiter, TRef}, Connection, State} ->
            erlang:cancel_timer(TRef),
            gen_server:reply(Waiter, {ok, Connection}),
            State;
        false ->
            State0
    end.

next_waiter_and_connection(#state{waiters=Waiters0, free=Free0}=State) ->
    case {queue:out(Waiters0), queue:out(Free0)} of
        {{{value, Waiter}, Waiters}, {{value, Connection}, Free}} ->
            {Waiter, Connection, State#state{waiters=Waiters, free=Free}};
        _ -> false
    end.

notify_connect(Pool, ConnectResult) ->
    gen_server:cast(Pool, {connect_result, ConnectResult}).

schedule_connect(_State) ->
    erlang:send(self(), connect).

schedule_next_connect(State) ->
    case pool_full(State) of
        true -> pool_full;
        false -> schedule_connect(State)
    end.

schedule_connect_retry(_State) ->
    Time = ?DEFAULT_RETRY_DELAY,
    error_logger:info_msg("Retrying connection in ~b millis~n", [Time]),
    erlang:send_after(Time, self(), connect).

pool_full(#state{max=Max, all=Connections}) ->
    length(Connections) >= Max.

add_connection(Connection, #state{all=All, free=Free}=State) ->
    erlang:monitor(process, Connection),
    error_logger:info_msg("Connection added to pool~n"),
    schedule_dispatch(),
    State#state{all=[Connection|All], free=queue:in(Connection, Free)}.

remove_connection(Connection, #state{all=All}=State) ->
    error_logger:info_msg("Removing connection from pool~n"),
    State#state{all=lists:delete(Connection, All)}.

release_connection(Connection, #state{free=Free, all=All}=State) ->
    case lists:member(Connection, All) of
        true ->
            schedule_dispatch(),
            State#state{free=queue:in(Connection, Free)};
        false ->
            State
    end.

add_waiter(Waiter, Timeout, #state{waiters=Waiters}=State) ->
    TRef = erlang:send_after(Timeout, self(), {acquire_timeout, Waiter}),
    State#state{waiters=queue:in({Waiter, TRef}, Waiters)}.

remove_waiter(Waiter, #state{waiters=Waiters}=State) ->
    State#state{waiters=queue_keydelete(Waiter, 1, Waiters)}.

schedule_dispatch() ->
    erlang:send(self(), dispatch).

queue_keydelete(Item, N, Queue) ->
    queue:from_list(lists:keydelete(Item, N, queue:to_list(Queue))).

select_db(_Connection, #state{db=undefined}) -> ok;
select_db(Connection, #state{db=N}) -> redis:select(Connection, N).
