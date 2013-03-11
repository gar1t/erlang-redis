-module(redis_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-define(POOL, test).
-define(POOL_SIZE, 10).
-define(CONCURRENT_READ_WRITERS, 100).
-define(READ_WRITE_COUNT, 10).
-define(CONCURRENCY_TEST_TIMEOUT, 60).

init_pool() ->
    redis:start(),
    {ok, PoolSup} = redis_pool_sup:start_link(?POOL, [{size, ?POOL_SIZE}]),
    {PoolSup, redis_tests:get_test_db()}.

cleanup_pool({PoolSup, _TestDB}) ->
    exit(PoolSup, shutdown).

concurrency_test_() ->
    {setup, fun init_pool/0, fun cleanup_pool/1,
     fun(X) ->
             {timeout, ?CONCURRENCY_TEST_TIMEOUT,
              fun() -> concurrent_read_writes(X) end}
     end}.

concurrent_read_writes({_, TestDB}) ->
    error_logger:info_msg("Testing concurrent read/writes using pool~n"),
    Parent = self(),
    Pids = [spawn_link(
              fun() ->
                      concurrent_read_write(I, TestDB),
                      Parent ! {done, self()}
              end)
            || I <- lists:seq(1, ?CONCURRENT_READ_WRITERS)],
    wait_for(Pids).

wait_for([]) -> ok;
wait_for([Pid|Rest]) ->
    receive
        {done, Pid} -> wait_for(Rest)
    end.

concurrent_read_write(I, TestDB) ->
    T0 = erlang:now(),
    {ok, C} = redis_pool:acquire(?POOL, ?CONCURRENCY_TEST_TIMEOUT * 1000),
    T1 = erlang:now(),
    io:format(user, "[~b]", [timer:now_diff(T1, T0) div 1000]),
    try
        concurrent_read_write(C, I, TestDB)
    after
        redis_pool:release(?POOL, C)
    end.

concurrent_read_write(C, I, TestDB) ->
    ok = redis:select(C, TestDB),
    SetKey = "set" ++ integer_to_list(I),
    redis:del(C, SetKey),
    lists:foreach(
      fun(N) ->
              true = redis:hset(C, SetKey, "field" ++ integer_to_list(N), N),
              io:format(user, "w", [])
      end, lists:seq(1, ?READ_WRITE_COUNT)),
    lists:foreach(
      fun(N) ->
              {ok, Bin} = redis:hget(C, SetKey, "field" ++ integer_to_list(N)),
              N = list_to_integer(binary_to_list(Bin)),
              io:format(user, "r", [])
      end, lists:seq(1, ?READ_WRITE_COUNT)).
