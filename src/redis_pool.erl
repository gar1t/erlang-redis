-module(redis_pool).

-behaviour(gen_server).

-export([start_link/1, start_link/2,
         acquire/1, acquire/2, release/2,
         with_db/3, with_db/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name, all, free, connecting, max,
                waiters, connect_options, db}).

-define(DEFAULT_RETRY_DELAY, 5000).
-define(DEFAULT_SIZE, 1).
-define(DEFAULT_CONNECT, []).
-define(DEFAULT_DATABASE, undefined).
-define(DEFAULT_ACQUIRE_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Options) ->
    gen_server:start_link(?MODULE, ['$anonymous', Options], []).

start_link(PoolName, Options) ->
    gen_server:start_link(
      {local, server_name(PoolName)}, ?MODULE, [PoolName, Options], []).

acquire(Pool) ->
    gen_server:call(
      server_name(Pool), {acquire, ?DEFAULT_ACQUIRE_TIMEOUT}, infinity).

acquire(Pool, Timeout) when is_integer(Timeout) ->
    gen_server:call(server_name(Pool), {acquire, Timeout}, infinity).

release(Pool, Connection) ->
    gen_server:cast(server_name(Pool), {release, Connection}).

with_db(Pool, WithDb, OnError) ->
    with_db(Pool, WithDb, OnError, ?DEFAULT_ACQUIRE_TIMEOUT).

with_db(Pool, WithDb, OnError, Timeout) when is_integer(Timeout) ->
    case acquire(Pool, Timeout) of
        {ok, C} ->
            try
                WithDb(C)
            after
                release(Pool, C)
            end;
        {error, Err} -> OnError(Err)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Options]) ->
    process_flag(trap_exit, true),
    State = init_state(Name, Options),
    {ok, schedule_connect(State)}.

handle_call({acquire, Timeout}, From, State) ->
    schedule_dispatch(),
    {noreply, add_waiter(From, Timeout, State)}.

handle_cast({release, Connection}, State) ->
    {noreply, release_connection(Connection, State)}.

handle_info(connect, #state{connecting=undefined}=State) ->
    {noreply, maybe_connect(State)};
handle_info(connect, State) ->
    {noreply, State};
handle_info(dispatch, State) ->
    {noreply, dispatch_connection(State)};
handle_info({acquire_timeout, Waiter}, State) ->
    gen_server:reply(Waiter, {error, timeout}),
    {noreply, remove_waiter(Waiter, State)};
handle_info({'EXIT', Pid, Result}, #state{connecting=Pid}=State) ->
    handle_connect_result(Result, State);
handle_info({'EXIT', Connection, Reason}, State) ->
    {noreply, schedule_connect(remove_connection(Connection, Reason, State))}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

server_name(Pid) when is_pid(Pid) -> Pid;
server_name(Pool) ->
    list_to_atom("redis_pool_" ++ atom_to_list(Pool)).

init_state(Name, Options) ->
    Max = proplists:get_value(size, Options, ?DEFAULT_SIZE),
    ConnectOptions =
        proplists:get_value(connect, Options, ?DEFAULT_CONNECT),
    Db = proplists:get_value(database, Options, ?DEFAULT_DATABASE),
    #state{name=Name,
           max=Max,
           connect_options=ConnectOptions,
           db=Db,
           all=[],
           free=queue:new(),
           waiters=queue:new()}.

maybe_connect(State) ->
    case pool_full(State) of
        true -> State;
        false -> start_connect_link(State)
    end.

start_connect_link(#state{connect_options=Options}=State) ->
    log(info, {connect_attempt, Options}, State),
    ConnectingPid = spawn_link(fun() -> connect(Options) end),
    State#state{connecting=ConnectingPid}.

connect(ClientOptions) ->
    try redis_client_sup:start_connection(ClientOptions) of
        Result -> exit(Result)
    catch
        T:E -> exit({error, {T, E, erlang:get_stacktrace()}})
    end.

dispatch_connection(State0) ->
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

schedule_connect(State) ->
    erlang:send(self(), connect),
    State.

schedule_connect_retry(State) ->
    Time = ?DEFAULT_RETRY_DELAY,
    log(info, {connect_retry_scheduled, Time}, State),
    erlang:send_after(Time, self(), connect),
    State.

schedule_dispatch() ->
    erlang:send(self(), dispatch).

pool_full(#state{max=Max, all=Connections}) ->
    length(Connections) >= Max.

handle_connect_result({ok, Connection}, State) ->
    select_db(Connection, State),
    {noreply, clear_connecting(
                schedule_connect(
                  add_connection(Connection, State)))};
handle_connect_result({error, Err}, State) ->
    log(error, {connect_error, Err}, State),
    {noreply, clear_connecting(schedule_connect_retry(State))}.

add_connection(Connection, #state{all=All, free=Free}=State) ->
    link(Connection),
    log(info, {connection_added, Connection}, State),
    schedule_dispatch(),
    State#state{all=[Connection|All], free=queue:in(Connection, Free)}.

remove_connection(Connection, Reason, #state{all=All, free=Free}=State) ->
    log(info, {connection_removed, Connection, Reason}, State),
    State#state{
      all=lists:delete(Connection, All),
      free=queue_delete(Connection, Free)}.

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

queue_keydelete(Item, N, Queue) ->
    queue:from_list(lists:keydelete(Item, N, queue:to_list(Queue))).

queue_delete(Item, Queue) ->
    queue:from_list(lists:delete(Item, queue:to_list(Queue))).

select_db(_Connection, #state{db=undefined}) -> ok;
select_db(Connection, #state{db=N}) -> redis:select(Connection, N).

clear_connecting(State) ->
    State#state{connecting=undefined}.

log(error, Report, State) ->
    error_logger:error_report(format_log_report(Report, State));
log(info, Report, State) ->
    error_logger:info_report(format_log_report(Report, State)).

format_log_report(Report, #state{name=Name}) ->
    {redis_pool, pool_ref(Name), Report}.

pool_ref('$anonymous') -> self();
pool_ref(Name) -> Name.
