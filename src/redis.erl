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
         setnx/3,
         exists/2,
         del/2,
         mdel/2,
         keys/2,
         quit/1,
         flushdb/1]).

%% TODO:
%%
%% APPEND
%% BGREWRITEAOF
%% BGSAVE
%% BLPOP
%% BRPOP
%% BRPOPLPUSH
%% CONFIG GET
%% CONFIG SET
%% CONFIG RESETAT
%% DEBUG OBJECT
%% DEBUG SEGFAULT
%% DECR
%% DECRBY
%% DEL
%% DISCARD
%% ECHO
%% EXEC
%% EXPIRE
%% EXPIREAT
%% FLUSHALL
%% GETBIT
%% GETRANGE
%% GETSET
%% HDEL
%% HEXISTS
%% HGET
%% HGETALL
%% HINCRBY
%% HKEYS
%% HLEN
%% HMGET
%% HMSET
%% HSET
%% HSETNX
%% HVALS
%% INCR
%% INCRBY
%% INFO
%% LASTSAVE
%% LINDEX
%% LINSERT
%% LLEN
%% LPOP
%% LPUSH
%% LPUSHX
%% LRANGE
%% LREM
%% LSET
%% LTRIM
%% MGET
%% MONITOR
%% MOVE
%% MSET
%% MSETNX
%% MULTI
%% OBJECT
%% PERSIST
%% PING
%% PSUBSCRIBE
%% PUBLISH
%% PUNSUBSCRIBE
%% RANDOMKEY
%% RENAME
%% RENAMENX
%% RPOP
%% RPOPLPUSH
%% RPUSH
%% RPUSHX
%% SADD
%% SAVE
%% SCARD
%% SDIFF
%% SDIFFSTORE
%% SELECT
%% SETBIT
%% SETEX
%% SETRANGE
%% SHUTDOWN    %% can use this to terminate test server?
%% SINTER
%% SINTERSTORE
%% SISMEMBER
%% SLAVEOF
%% SLOWLOG
%% SMEMBERS
%% SMOVE
%% SORT
%% SPOP
%% SRANDMEMBER
%% SREM
%% STRLEN
%% SUBSCRIBE
%% SUNINION
%% SUNIONSTORE
%% SYNC
%% TTL
%% TYPE
%% UNSUBSCRIBE
%% UNWATCH
%% WATCH
%% ZADD
%% ZCARD
%% ZCOUNT
%% ZINCRBY
%% ZINTERSTORE
%% ZRANGE
%% ZRANGEBYSCORE
%% ZRANK
%% ZREM
%% ZREMRANKGEBYRANK
%% ZREMRANGEBYSCORE
%% ZREVRANGE
%% ZREVRANGEBYSCORE
%% ZREVRANK
%% ZSCORE
%% ZUNIONSTORE

-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, 6379).

-define(is_string(Val), is_list(Val) orelse is_binary(Val)).

%%%===================================================================
%%% Public API
%%%===================================================================

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
%% @spec set(Server, Key, Value) -> ok
%% Server = server()
%% Key = key()
%% Value = binary()
%% @end
%%--------------------------------------------------------------------

set(Server, Key, Value) when ?is_string(Key), is_binary(Value) ->
    case redis_client:request(Server, {"SET", [Key, Value]}) of
        ok -> ok;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Set Key to Value only if Key doesn't already exist.
%%
%% Returns true if the value was set, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/setnx SETNX]
%%
%% @spec setnx(Server, Key, Value) -> boolean()
%% @end
%%--------------------------------------------------------------------

setnx(Server, Key, Value) when ?is_string(Key), is_binary(Value) ->
    case redis_client:request(Server, {"SETNX", [Key, Value]}) of
        {ok, 0} -> false;
        {ok, 1} -> true;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Returns true if Key exists, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/exists EXISTS]
%%
%% @spec exists(Server, Key) -> boolean()
%% Server = server()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

exists(Server, Key) when ?is_string(Key) ->
    case redis_client:request(Server, {"EXISTS", [Key]}) of
        {ok, 0} -> false;
        {ok, 1} -> true;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Removes the specified key.
%%
%% Returns true if the key was deleted, otherwise returns false.
%%
%% A key is ignored if it does not exist.
%%
%% This function deletes a single key. To delete multiple keys, use
%% `mdel/2'.
%%
%% Redis command: [http://redis.io/commands/del DEL]
%%
%% @spec del(Server, Key) -> Deleted
%% Server = server()
%% Key = key()
%% Deleted = boolean()
%% @see mdel/2
%% @end
%%--------------------------------------------------------------------

del(Server, Key) when ?is_string(Key) ->
    case redis_client:request(Server, {"DEL", [Key]}) of
        {ok, 0} -> false;
        {ok, 1} -> true;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Removes a list of keys.
%%
%% Returns a count of keys deleted.
%%
%% A key is ignored if it does not exist.
%%
%% This function deletes multiple keys. To delete a single key, use
%% `del/2'.
%%
%% Redis command: [http://redis.io/commands/del DEL]
%%
%% @spec mdel(Server, Keys) -> Deleted
%% Server = server()
%% Keys = [key()]
%% Deleted = integer()
%% @see del/2
%% @end
%%--------------------------------------------------------------------

mdel(Server, Keys) when length(Keys) > 0 ->
    validate_keys(Keys),
    case redis_client:request(Server, {"DEL", Keys}) of
        {ok, Deleted} -> Deleted;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Returns a list of keys matching Pattern.
%%
%% Redis command: [http://redis.io/commands/keys KEYS]
%%
%% @spec keys(Server, Pattern) -> Keys
%% Server = server()
%% Pattern = string()
%% Keys = [binary()]
%% @end
%%--------------------------------------------------------------------

keys(Server, Pattern) ->
    case redis_client:request(Server, {"KEYS", [Pattern]}) of
        {ok, Keys} -> Keys;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Ask the server to close the connection.
%%
%% The connection is closed as soon as all pending replies have been
%% written to the client.
%%
%% Redis command: [http://redis.io/commands/quit QUIT]
%%
%% @spec quit(Server) -> ok
%% Server = server()
%% @end
%%--------------------------------------------------------------------

quit(Server) ->
    redis_client:quit(Server).

%%--------------------------------------------------------------------
%% @doc Delete all the keys in the selected DB.
%%
%% Redis command: [http://redis.io/commands/flushdb FLUSHDB]
%%
%% @spec flushdb(Server) -> ok
%% @end
%%--------------------------------------------------------------------

flushdb(Server) ->
    case redis_client:request(Server, {"FLUSHDB", []}) of
        ok -> ok;
        {error, Err} -> error(Err)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_keys([]) -> ok;
validate_keys([Key|Rest]) when ?is_string(Key) -> validate_keys(Rest);
validate_keys([Bad|_]) -> error({badkey, Bad}).
