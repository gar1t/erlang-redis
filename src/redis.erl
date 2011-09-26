-module(redis).

-export([connect/0, connect/1, connect/2,
         auth/2,
         dbsize/1,
         get/2,
         set/3]).

-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, 6379).

-define(is_string(Val), is_list(Val) orelse is_binary(Val)).

connect() ->
    redis_client:start_link(?DEFAULT_HOST, ?DEFAULT_PORT).

connect(Host) ->
    redis_client:start_link(Host, ?DEFAULT_PORT).

connect(Host, Port) ->
    redis_client:start_link(Host, Port).

auth(Client, Password) when ?is_string(Password) ->
    redis_client:request(Client, {"AUTH", [Password]}).

dbsize(Client) ->
    redis_client:request(Client, {"DBSIZE", []}).

get(Client, Key) when ?is_string(Key) ->
    redis_client:request(Client, {"GET", [Key]}).

set(Client, Key, Value) when ?is_string(Key), is_binary(Value) ->
    redis_client:request(Client, {"SET", [Key, Value]}).
