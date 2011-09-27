%%% @author Garrett Smith <g@rre.tt>
%%% @doc API module for Redis.
%%%
%%% @type client() = pid()
%%% @type iolist() = binary() | string()
%%% @type key() = iolist()
%%% @type value() = iolist() | integer()
%%% @type stored_value() = binary()
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
         flushdb/1,
         append/3,
         echo/2,
         ping/1,
         decr/2,
         decrby/3,
         incr/2,
         incrby/3,
         rename/3,
         renamenx/3,
         sadd/3,
         smadd/3,
         srem/3,
         smrem/3,
         sismember/3,
         smembers/2]).

%% TODO:
%%
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
%% DISCARD
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
%% PSUBSCRIBE
%% PUBLISH
%% PUNSUBSCRIBE
%% RANDOMKEY
%% RPOP
%% RPOPLPUSH
%% RPUSH
%% RPUSHX
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
%% SLAVEOF
%% SLOWLOG
%% SMOVE
%% SORT
%% SPOP
%% SRANDMEMBER[5~
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

-define(ok(Val),
        case Val of
            ok -> ok;
            {error, Err} -> error(Err)
        end).

-define(bool(Val),
        case Val of
            {ok, 0} -> false;
            {ok, 1} -> true;
            {error, Err} -> error(Err)
        end).

-define(term(Val),
        case Val of
            {ok, Term} -> Term;
            {error, Err} -> error(Err)
        end).

%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Connect to a locally running Redis server.
%% @spec connect() -> {ok, Client}  | {error, Reason}
%% Client = client()
%% @equiv connect("127.0.0.1")
%% @end
%%--------------------------------------------------------------------

connect() ->
    redis_client:start_link(?DEFAULT_HOST, ?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Connect to a host running Redis on the standard Redis port.
%% @spec connect(Host) -> {ok, Client} | {error, Reason}
%% Host = string()
%% Client = client()
%% @equiv connect(Host, 6379)
%% @end
%%--------------------------------------------------------------------

connect(Host) ->
    redis_client:start_link(Host, ?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Connect to a host running Redis on a non-standard port.
%% @spec connect(Host, Port) -> {ok, Client} | {error, Reason}
%% Host = string()
%% Port = integer()
%% Client = client()
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
%% @spec auth(Client, Password) -> ok | {error, Reason}
%% Client = client()
%% Password = string()
%% @end
%%--------------------------------------------------------------------

auth(Client, Password) ->
    redis_client:request(Client, {"AUTH", [Password]}).

%%--------------------------------------------------------------------
%% @doc Returns the number of keys in the selected database.
%%
%% Redis command: [http://redis.io/commands/dbsize DBSIZE]
%%
%% @spec dbsize(Client) -> integer()
%% @end
%%--------------------------------------------------------------------

dbsize(Client) ->
    ?term(redis_client:request(Client, {"DBSIZE", []})).

%%--------------------------------------------------------------------
%% @doc Gets the value of Key or undefined if Key does not exist.
%%
%% Generates an error if the value of Key exists but is not a binary.
%%
%% Redis command: [http://redis.io/commands/get GET]
%%
%% @spec get(Client, Key) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

get(Client, Key) ->
    case redis_client:request(Client, {"GET", [Key]}) of
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
%% @spec set(Client, Key, Value) -> ok
%% Client = client()
%% Key = key()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

set(Client, Key, Value) ->
    ?ok(redis_client:request(Client, {"SET", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Set Key to Value only if Key doesn't already exist.
%%
%% Returns true if the value was set, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/setnx SETNX]
%%
%% @spec setnx(Client, Key, Value) -> bool()
%% @end
%%--------------------------------------------------------------------

setnx(Client, Key, Value) ->
    ?bool(redis_client:request(Client, {"SETNX", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Returns true if Key exists, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/exists EXISTS]
%%
%% @spec exists(Client, Key) -> bool()
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

exists(Client, Key) ->
    ?bool(redis_client:request(Client, {"EXISTS", [Key]})).

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
%% @spec del(Client, Key) -> bool()
%% Client = client()
%% Key = key()
%% @see mdel/2
%% @end
%%--------------------------------------------------------------------

del(Client, Key) ->
    ?bool(redis_client:request(Client, {"DEL", [Key]})).

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
%% @spec mdel(Client, Keys) -> integer()
%% Client = client()
%% Keys = [key()]
%% @see del/2
%% @end
%%--------------------------------------------------------------------

mdel(Client, Keys) when length(Keys) > 0 ->
    ?term(redis_client:request(Client, {"DEL", Keys})).

%%--------------------------------------------------------------------
%% @doc Returns a list of keys matching Pattern.
%%
%% Redis command: [http://redis.io/commands/keys KEYS]
%%
%% @spec keys(Client, Pattern) -> Keys
%% Client = client()
%% Pattern = string()
%% Keys = [key()]
%% @end
%%--------------------------------------------------------------------

keys(Client, Pattern) ->
    ?term(redis_client:request(Client, {"KEYS", [Pattern]})).

%%--------------------------------------------------------------------
%% @doc Ask the server to close the connection.
%%
%% The connection is closed as soon as all pending replies have been
%% written to the client.
%%
%% Redis command: [http://redis.io/commands/quit QUIT]
%%
%% @spec quit(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

quit(Client) ->
    redis_client:quit(Client).

%%--------------------------------------------------------------------
%% @doc Delete all the keys in the selected DB.
%%
%% Redis command: [http://redis.io/commands/flushdb FLUSHDB]
%%
%% @spec flushdb(Client) -> ok
%% @end
%%--------------------------------------------------------------------

flushdb(Client) ->
    ?ok(redis_client:request(Client, {"FLUSHDB", []})).

%%--------------------------------------------------------------------
%% @doc Appends Value to existing value for Key.
%%
%% If Key does not exist, same as `set/3'.
%%
%% Returns the length of the new value for Key.
%%
%% Redis command: [http://redis.io/commands/append APPEND]
%%
%% @spec append(Client, Key, Value) -> integer()
%% Client = client()
%% Key = key()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

append(Client, Key, Value) ->
    ?term(redis_client:request(Client, {"APPEND", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Returns a message.
%%
%% Redis command: [http://redis.io/commands/echo ECHO]
%%
%% @spec echo(Client, Msg) -> binary()
%% Client = client()
%% Msg = value()
%% @end
%%--------------------------------------------------------------------

echo(Client, Msg) ->
    ?term(redis_client:request(Client, {"ECHO", [Msg]})).

%%--------------------------------------------------------------------
%% @doc Pings the server.
%%
%% Redis command: [http://redis.io/commands/ping PING]
%%
%% @spec ping(Client) -> pong
%% Client = client()
%% @end
%%--------------------------------------------------------------------

ping(Client) ->
    case redis_client:request(Client, {"PING", []}) of
        {ok, "PONG"} -> pong;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Decrements the number stored at Key.
%%
%% Returns the new integer representation of Key.
%%
%% If Key does not exist, it's assumed to be 0 and then decremented.
%%
%% If Key exists and is the wrong type or is not representable as an
%% integer, generates an error.
%%
%% Redis command: [http://redis.io/commands/decr DECR]
%%
%% @spec decr(Client, Key) -> integer()
%% Client = client()
%% Key = key()
%% @see decrby/3
%% @end
%%--------------------------------------------------------------------

decr(Client, Key) ->
    ?term(redis_client:request(Client, {"DECR", [Key]})).

%%--------------------------------------------------------------------
%% @doc Decrements the number stored as Key.
%%
%% Same as `decr/2' with an explicit Decrement value.
%%
%% Redis command: [http://redis.io/commands/decrby DECRBY]
%%
%% @spec decrby(Client, Key, Decrement) -> integer()
%% Client = client()
%% Key = key()
%% Decrement = integer()
%% @see decr/2
%% @end
%%--------------------------------------------------------------------

decrby(Client, Key, Decrement) ->
    ?term(redis_client:request(Client, {"DECRBY", [Key, Decrement]})).

%%--------------------------------------------------------------------
%% @doc Increments the number stored at Key.
%%
%% Returns the new integer representation of Key.
%%
%% If Key does not exist, it's assumed to be 0 and then incremented.
%%
%% If Key exists and is the wrong type or is not representable as an
%% integer, generates an error.
%%
%% Redis command: [http://redis.io/commands/incr INCR]
%%
%% @spec incr(Client, Key) -> integer()
%% Client = client()
%% Key = key()
%% @see incrby/3
%% @end
%%--------------------------------------------------------------------

incr(Client, Key) ->
    ?term(redis_client:request(Client, {"INCR", [Key]})).

%%--------------------------------------------------------------------
%% @doc Increments the number stored as Key.
%%
%% Same as `incr/2' with an explicit Increment value.
%%
%% Redis command: [http://redis.io/commands/incryb INCRBY]
%%
%% @spec incrby(Client, Key, Increment) -> integer()
%% Client = client()
%% Key = key()
%% Increment = integer()
%% @see incr/2
%% @end
%%--------------------------------------------------------------------

incrby(Client, Key, Increment) ->
    ?term(redis_client:request(Client, {"INCRBY", [Key, Increment]})).

%%--------------------------------------------------------------------
%% @doc Renames Key to NewKey.
%%
%% Generates an error if Key doesn't exist.
%%
%% Redis command: [http://redis.io/commands/rename RENAME]
%%
%% @spec rename(Client, Key, NewKey) -> ok
%% Client = client()
%% Key = key()
%% NewKey = key()
%% @end
%%--------------------------------------------------------------------

rename(Client, Key, NewKey) ->
    ?ok(redis_client:request(Client, {"RENAME", [Key, NewKey]})).

%%--------------------------------------------------------------------
%% @doc Renames Key to NewKey only if NewKey doesn't already exist.
%%
%% Returns true if Key was renamed, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/renamenx RENAMENX]
%%
%% @spec renamenx(Client, Key, NewKey) -> bool()
%% Client = client()
%% Key = key()
%% NewKey = key()
%% @end
%%--------------------------------------------------------------------

renamenx(Client, Key, NewKey) ->
    ?bool(redis_client:request(Client, {"RENAMENX", [Key, NewKey]})).

%%--------------------------------------------------------------------
%% @doc Adds a member to a set under Key.
%%
%% Returns true if the member was added, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/sadd SADD]
%%
%% @spec sadd(Client, Key, Member) -> bool()
%% Client = client()
%% Key = key()
%% Member = value()
%% @see smadd/3
%% @end
%%--------------------------------------------------------------------

sadd(Client, Key, Member) ->
    ?bool(redis_client:request(Client, {"SADD", [Key, Member]})).

%%--------------------------------------------------------------------
%% @doc Adds multiple members to a set under Key.
%%
%% Returns the number of new members added to Key.
%%
%% Redis version: >= 2.4
%%
%% Redis command: [http://redis.io/commands/sadd SADD]
%%
%% @spec smadd(Client, Key, Members) -> integer()
%% Client = client()
%% Key = key()
%% Members = [value()]
%% @see sadd/3
%% @end
%%--------------------------------------------------------------------

smadd(Client, Key, Members) when length(Members) > 0 ->
    ?term(redis_client:request(Client, {"SADD", [Key|Members]})).

%%--------------------------------------------------------------------
%% @doc Removes a member from a set.
%%
%% Returns true if the member was removed, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/srem SREM]
%%
%% @spec srem(Client, Key, Member) -> boolean()
%% Client = client()
%% Key = key()
%% Member = value()
%% @end
%%--------------------------------------------------------------------

srem(Client, Key, Member) ->
    ?bool(redis_client:request(Client, {"SREM", [Key, Member]})).

%%--------------------------------------------------------------------
%% @doc Removes multiple members from a set.
%%
%% Returns the number of members removed.
%%
%% Redis version: >= 2.4
%%
%% Redis command: [http://redis.io/commands/srem SREM]
%%
%% @spec smrem(Client, Key, Members) -> integer()
%% Client = client()
%% Key = key()
%% Members = [value()]
%% @end
%%--------------------------------------------------------------------

smrem(Client, Key, Members) ->
    ?bool(redis_client:request(Client, {"SREM", [Key|Members]})).

%%--------------------------------------------------------------------
%% @doc Returns true if Member is a part of a set, otherwise returns
%% false.
%%
%% Redis command: [http://redis.io/commands/sismember SISMEMBER]
%%
%% @spec sismember(Client, Key, Member) -> boolean()
%% Client = client()
%% Key = key()
%% Member = value()
%% @end
%%--------------------------------------------------------------------

sismember(Client, Key, Member) ->
    ?bool(redis_client:request(Client, {"SISMEMBER", [Key, Member]})).

%%--------------------------------------------------------------------
%% @doc Returns the list of members for a set.
%%
%% Redis command: [http://redis.io/commands/smembers SMEMBERS]
%%
%% @spec smembers(Client, Key) -> Members
%% Client = client()
%% Key = key()
%% Members = [stored_value()]
%% @end
%%--------------------------------------------------------------------

smembers(Client, Key) ->
    ?term(redis_client:request(Client, {"SMEMBERS", [Key]})).

%%%===================================================================
%%% Internal functions
%%%===================================================================
