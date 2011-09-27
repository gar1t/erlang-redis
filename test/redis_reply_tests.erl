-module(redis_reply_tests).

-include_lib("eunit/include/eunit.hrl").

integer_test() ->
    %% Provide enough to parse an int
    ?assertEqual({{value, {ok, 123}}, {unknown, <<>>}},
                 redis_reply:data(<<":123\r\n">>, {unknown, <<>>})),

    %% Build up and pass the int
    ?assertEqual({pending, {integer, <<"1">>}},
                 redis_reply:data(<<":1">>, {unknown, <<>>})),
    ?assertEqual({{value, {ok, 1}}, {unknown, <<>>}},
                 redis_reply:data(<<"\r\n">>, {integer, <<"1">>})),
    ?assertEqual({{value, {ok, 1}}, {unknown, <<"xxx">>}},
                 redis_reply:data(<<"\r\nxxx">>, {integer, <<"1">>})),
    ok.

message_test() ->
    %% Just enough for a message
    ?assertEqual({{value, {ok, "OK"}}, {unknown, <<>>}},
                 redis_reply:data(<<"+OK\r\n">>, {unknown, <<>>})),

    %% Up to and past a message
    ?assertEqual({pending, {message, <<"O">>}},
                 redis_reply:data(<<"+O">>, {unknown, <<>>})),
    ?assertEqual({pending, {message, <<"OK\r">>}},
                 redis_reply:data(<<"K\r">>, {message, <<"O">>})),
    ?assertEqual({{value, {ok, "OK"}}, {unknown, <<"xxx">>}},
                 redis_reply:data(<<"\nxxx">>, {message, <<"OK\r">>})),
    ok.

error_test() ->
    %% Just enough for a message
    ?assertEqual({{value, {error, "ERR"}}, {unknown, <<>>}},
                 redis_reply:data(<<"-ERR\r\n">>, {unknown, <<>>})),

    %% Up to and past a message
    ?assertEqual({pending, {error, <<"ER">>}},
                 redis_reply:data(<<"-ER">>, {unknown, <<>>})),
    ?assertEqual({{value, {error, "ERR"}}, {unknown, <<>>}},
                 redis_reply:data(<<"R\r\n">>, {error, <<"ER">>})),
    ?assertEqual({{value, {error, "ERR"}}, {unknown, <<"xxx">>}},
                 redis_reply:data(<<"-ERR\r\nxxx">>, {unknown, <<>>})),
    ok.

bulk_test() ->
    %% Just enough for a bulk reply
    ?assertEqual({{value, {ok, <<"foo">>}}, {unknown, <<>>}},
                 redis_reply:data(<<"$3\r\nfoo\r\n">>, {unknown, <<>>})),

    %% Up to and past
    ?assertEqual({pending, {bulk, <<"3">>}},
                 redis_reply:data(<<"$3">>, {unknown, <<>>})),
    ?assertEqual({pending, {bulk, 3, <<"fo">>}},
                 redis_reply:data(<<"\r\nfo">>, {bulk, <<"3">>})),
    ?assertEqual({{value, {ok, <<"foo">>}}, {unknown, <<"$3\r\nb">>}},
                 redis_reply:data(<<"o\r\n$3\r\nb">>, {bulk, 3, <<"fo">>})),
    ?assertEqual({{value, {ok, <<"bar">>}}, {unknown, <<>>}},
                 redis_reply:data(<<"ar\r\n">>, {unknown, <<"$3\r\nb">>})),
    ok.

multi_test() ->
    %% Partial
    ?assertEqual({pending, {multi, <<"0">>}},
                 redis_reply:data(<<"*0">>, {unknown, <<>>})),

    %% Empty list
    ?assertEqual({{value, {ok, []}}, {unknown, <<>>}},
                 redis_reply:data(<<"*0\r\n">>, {unknown, <<>>})),

    %% Nil (undefined)
    ?assertEqual({{value, undefined}, {unknown, <<>>}},
                 redis_reply:data(<<"*-1\r\n">>, {unknown, <<>>})),

    %% 1 item
    ?assertEqual({{value, {ok, [<<"foo">>]}}, {unknown, <<>>}},
                 redis_reply:data(<<"*1\r\n",
                                    "$3\r\nfoo\r\n">>,
                                  {unknown, <<>>})),

    %% 2 items
    ?assertEqual({{value, {ok, [<<"foo">>, <<"bar">>]}}, {unknown, <<>>}},
                 redis_reply:data(<<"*2\r\n",
                                    "$3\r\nfoo\r\n",
                                    "$3\r\nbar\r\n">>,
                                  {unknown, <<>>})),
    ok.
