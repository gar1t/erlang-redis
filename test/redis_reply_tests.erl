-module(redis_reply_tests).

-include_lib("eunit/include/eunit.hrl").

parse_value(Data) ->
    {{value, Val}, _} = redis_reply:data(Data, redis_reply:new()),
    Val.

integer_test() ->
    ?assertEqual({ok, 123}, parse_value(<<":123\r\n">>)).

ok_test() ->
    ?assertEqual({ok, "OK"}, parse_value(<<"+OK\r\n">>)).

error_test() ->
    ?assertEqual({error, "ERR bang"}, parse_value(<<"-ERR bang\r\n">>)).
