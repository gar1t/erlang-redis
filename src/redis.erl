%%% @author Garrett Smith <g@rre.tt>
%%% @doc API module for Redis.
%%%
%%% @type server() = pid()
%%% @type key() = binary() | string()
%%% @end
-module(redis).

-export([connect/0, connect/1, connect/2,
         auth/2,
         dbsize/1,
         get/2,
         set/3,
         exists/2]).

-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, 6379).

-define(is_string(Val), is_list(Val) orelse is_binary(Val)).

%%--------------------------------------------------------------------
%% @doc Connect to a locally running Redis server.
%% @spec connect() -> {ok, Server}  | {error, Reason}
%% Server = server()
%% @equiv connect("127.0.0.1")
%% @end
%%--------------------------------------------------------------------

connect() ->
    redis_client:start_link(?DEFAULT_HOST, ?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Connect to a host running Redis on the standard Redis port.
%% @spec connect(Host) -> {ok, Server} | {error, Reason}
%% Host = string()
%% Server = server()
%% @equiv connect(Host, 6379)
%% @end
%%--------------------------------------------------------------------

connect(Host) ->
    redis_client:start_link(Host, ?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Connect to a host running Redis on a non-standard port.
%% @spec connect(Host, Port) -> {ok, Server} | {error, Reason}
%% Host = string()
%% Port = integer()
%% Server = server()
%% @end
%%--------------------------------------------------------------------

connect(Host, Port) ->
    redis_client:start_link(Host, Port).

%%--------------------------------------------------------------------
%% @doc Authenticate with a Redis server.
%%
%% If the server is configured with password protection, you must
%% first authenticate before running other commands.
%%
%% Redis command: [http://redis.io/commands/auth AUTH]
%%
%% @spec auth(Server, Password) -> ok | {error, Reason}
%% Server = server()
%% Password = string()
%% @end
%%--------------------------------------------------------------------

auth(Server, Password) when ?is_string(Password) ->
    redis_client:request(Server, {"AUTH", [Password]}).

%%--------------------------------------------------------------------
%% @doc Returns the number of keys in the selected database.
%%
%% Redis command: [http://redis.io/commands/dbsize DBSIZE]
%%
%% @spec dbsize(Server) -> integer()
%% @end
%%--------------------------------------------------------------------

dbsize(Server) ->
    case redis_client:request(Server, {"DBSIZE", []}) of
        {ok, Keys} -> Keys;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Gets the value of Key or undefined if Key does not exist.
%%
%% Generates an error if the value of Key exists but is not a binary.
%%
%% Redis command: [http://redis.io/commands/get GET]
%%
%% @spec get(Server, Key) -> {ok, Value} | undefined
%% Server = server()
%% Key = key()
%% Value = binary()
%% @end
%%--------------------------------------------------------------------

get(Server, Key) when ?is_string(Key) ->
    case redis_client:request(Server, {"GET", [Key]}) of
        {ok, Val} -> {ok, Val};
        undefined -> undefined;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Set key to hold the string value.
%%
%% If key already holds a value, it is overwritten, regardless of its type.
%%
%% Redis command: [http://redis.io/commands/set SET]
%%
%% @spec set(Server, Key, Value) -> ok | {error, Reason}
%% Server = server()
%% Key = key()
%% Value = binary()
%% @end
%%--------------------------------------------------------------------

set(Server, Key, Value) when ?is_string(Key), is_binary(Value) ->
    redis_client:request(Server, {"SET", [Key, Value]}).

%%--------------------------------------------------------------------
%% @doc Returns true if Key exists, otherwise false.
%%
%% Redis command: [http://redis.io/commands/exists EXISTS]
%%
%% @spec exists(Server, Key) -> boolean()
%% Server = server()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

exists(Server, Key) when ?is_string(Key) ->
    redis_client:request(Server, {"EXISTS", [Key]}).
