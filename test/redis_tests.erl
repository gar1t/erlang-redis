-module(redis_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_PORT, 6380).

start_redis() ->
    {ok, Server} = redis_test_server:start_link(?TEST_PORT),
    {ok, Client} = redis:connect("127.0.0.1", ?TEST_PORT),
    {Server, Client}.

stop_redis({Server, Client}) ->
    try
        ok = redis:quit(Client)
    after
        ok = redis_test_server:stop(Server),
        timer:sleep(100)
    end.

server_test_() ->
    [{setup, fun start_redis/0, fun stop_redis/1, {with, [Test]}}
     || Test <- [fun sniff/1,
                 fun setnx/1,
                 fun mdel/1]].

sniff({_, R}) ->
    ?assertEqual(0, redis:dbsize(R)),
    ?assertEqual(undefined, redis:get(R, "hello")),
    ?assertEqual(false, redis:exists(R, "hello")),
    ?assertEqual(ok, redis:set(R, "hello", <<"world">>)),
    ?assertEqual({ok, <<"world">>}, redis:get(R, "hello")),
    ?assertEqual(true, redis:exists(R, "hello")),
    ?assertEqual(true, redis:del(R, "hello")),
    ?assertEqual(undefined, redis:get(R, "hello")).

setnx({_, R}) ->
    ?assertEqual(0, redis:dbsize(R)),
    ?assertEqual(true, redis:setnx(R, "hello", <<"world">>)),
    ?assertEqual(false, redis:setnx(R, "hello", <<"world 2">>)),
    ?assertEqual({ok, <<"world">>}, redis:get(R, "hello")).

mdel({_, R}) ->
    ?assertEqual(0, redis:dbsize(R)),
    ?assertEqual(0, redis:mdel(R, ["foo", "bar", "baz"])),

    ?assertEqual(ok, redis:set(R, "foo", <<"Foo">>)),
    ?assertEqual(1, redis:mdel(R, ["foo", "bar", "baz"])),

    ?assertEqual(ok, redis:set(R, "foo", <<"Foo">>)),
    ?assertEqual(ok, redis:set(R, "bar", <<"Bar">>)),
    ?assertEqual(2, redis:mdel(R, ["foo", "bar", "baz"])),

    ?assertEqual(ok, redis:set(R, "foo", <<"Foo">>)),
    ?assertEqual(ok, redis:set(R, "bar", <<"Bar">>)),
    ?assertEqual(ok, redis:set(R, "baz", <<"Baz">>)),
    ?assertEqual(3, redis:mdel(R, ["foo", "bar", "baz"])).
