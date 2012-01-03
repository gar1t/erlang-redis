-module(redis_tests).

-include_lib("eunit/include/eunit.hrl").

-export([get_test_db/0]).

init_client() ->
    DB = get_test_db(),
    {ok, C} = redis:connect(),
    ok = redis:select(C, DB),
    ok = redis:flushdb(C),
    C.

get_test_db() ->
    case os:getenv("TESTDB") of
        false -> error("TESTDB env var not set");
        Val ->
            try list_to_integer(Val) of
                I when I >= 0, I =< 15 -> I;
                _ -> error("Invalid TESTDB - must be 0 - 15")
            catch
                error:badarg ->
                    error("Invalid TESTDB - must be an integer")
            end
    end.

quit_client(Client) ->
    ok = redis_client:quit(Client).

server_test_() ->
    [{setup, fun init_client/0, fun quit_client/1, {with, [T]}}
     || T <- [fun sniff/1,
              fun setnx/1,
              fun mdel/1]].

sniff(C) ->
    ?assertEqual(0, redis:dbsize(C)),
    ?assertEqual(undefined, redis:get(C, "hello")),
    ?assertEqual(false, redis:exists(C, "hello")),
    ?assertEqual(ok, redis:set(C, "hello", <<"world">>)),
    ?assertEqual({ok, <<"world">>}, redis:get(C, "hello")),
    ?assertEqual(true, redis:exists(C, "hello")),
    ?assertEqual(true, redis:del(C, "hello")),
    ?assertEqual(undefined, redis:get(C, "hello")).

setnx(C) ->
    ?assertEqual(0, redis:dbsize(C)),
    ?assertEqual(true, redis:setnx(C, "hello", <<"world">>)),
    ?assertEqual(false, redis:setnx(C, "hello", <<"world 2">>)),
    ?assertEqual({ok, <<"world">>}, redis:get(C, "hello")).

mdel(C) ->
    ?assertEqual(0, redis:dbsize(C)),
    ?assertEqual(0, redis:mdel(C, ["foo", "bar", "baz"])),

    ?assertEqual(ok, redis:set(C, "foo", <<"Foo">>)),
    ?assertEqual(1, redis:mdel(C, ["foo", "bar", "baz"])),

    ?assertEqual(ok, redis:set(C, "foo", <<"Foo">>)),
    ?assertEqual(ok, redis:set(C, "bar", <<"Bar">>)),
    ?assertEqual(2, redis:mdel(C, ["foo", "bar", "baz"])),

    ?assertEqual(ok, redis:set(C, "foo", <<"Foo">>)),
    ?assertEqual(ok, redis:set(C, "bar", <<"Bar">>)),
    ?assertEqual(ok, redis:set(C, "baz", <<"Baz">>)),
    ?assertEqual(3, redis:mdel(C, ["foo", "bar", "baz"])).
