%%% @author Garrett Smith <g@rre.tt>
%%% @doc API module for Redis.
%%%
%%% @type client() = pid()
%%% @type iolist() = binary() | string()
%%% @type key() = iolist()
%%% @type field() = iolist()
%%% @type channel() = iolist()
%%% @type pattern() = iolist()
%%% @type value() = iolist() | integer()
%%% @type stored_value() = binary()
%%% @type stored_key() = binary()
%%% @type stored_field() = binary()
%%% @type scored_member() = {Score, Member}
%%% @type score() = number()
%%% @type member() = value()
%%% @type aggregate() = [min | max | sum]
%%% @end
-module(redis).

-include("redis.hrl").

-export([connect/0, connect/1,
         append/3,
         auth/2,
         bgrewriteaof/1,
         bgsave/1,
         blpop/3,
         bmlpop/3,
         bmrpop/3,
         brpop/3,
         brpoplpush/4,
         config_get/2,
         config_resetstat/1,
         config_set/3,
         dbsize/1,
         debug_object/2,
         debug_segfault/1,
         decr/2,
         decrby/3,
         del/2,
         discard/1,
         echo/2,
         exec/1,
         exists/2,
         expire/3,
         expireat/3,
         flushall/1,
         flushdb/1,
         get/2,
         getbit/3,
         getrange/4,
         getset/3,
         hdel/3,
         hexists/3,
         hget/3,
         hgetall/2,
         hincrby/4,
         hkeys/2,
         hlen/2,
         hmget/3,
         hmset/3,
         hset/4,
         hsetnx/4,
         hvals/2,
         incr/2,
         incrby/3,
         info/1,
         keys/2,
         lastsave/1,
         lindex/3,
         linsert/5,
         llen/2,
         lpop/2,
         lpush/3,
         lpushx/3,
         lrange/4,
         lrem/4,
         lset/4,
         ltrim/4,
         mdel/2,
         mget/2,
         mlpush/3,
         monitor/1,
         move/3,
         mrpush/3,
         mset/2,
         msetnx/2,
         multi/1,
         object/3,
         persist/2,
         ping/1,
         psubscribe/2,
         publish/3,
         punsubscribe/1,
         punsubscribe/2,
         quit/1,
         randomkey/1,
         rename/3,
         renamenx/3,
         rpop/2,
         rpoplpush/3,
         rpush/3,
         rpushx/3,
         sadd/3,
         save/1,
         scard/2,
         sdiff/2,
         sdiffstore/3,
         select/2,
         set/3,
         setbit/4,
         setex/4,
         setnx/3,
         setrange/4,
         shutdown/1,
         sinter/2,
         sinterstore/3,
         sismember/3,
         slaveof/2,
         slowlog/2,
         smadd/3,
         smembers/2,
         smove/4,
         smrem/3,
         sort/2,
         sort/3,
         spop/2,
         srandmember/2,
         srem/3,
         strlen/2,
         subscribe/2,
         sunion/2,
         sunionstore/3,
         sync/1,
         ttl/2,
         type/2,
         unsubscribe/1,
         unsubscribe/2,
         unwatch/1,
         watch/2,
         zmadd/3,
         zadd/3,
         zadd/4,
         zcard/2,
         zcount/4,
         zincrby/4,
         zinterstore/3,
         zinterstore/4,
         zrange/4,
         zrange/5,
         zrangebyscore/4,
         zrangebyscore/5,
         zrank/3,
         zmrem/3,
         zrem/3,
         zremrangebyrank/4,
         zremrangebyscore/4,
         zrevrange/4,
         zrevrange/5,
         zrevrangebyscore/4,
         zrevrangebyscore/5,
         zrevrank/3,
         zscore/3,
         zunionstore/3,
         zunionstore/4
        ]).

-define(ok(_Val),
        case _Val of
            ok -> ok;
            {ok, _} -> ok;
            {error, _Err} -> error(_Err)
        end).

-define(bool(_Val),
        case _Val of
            {ok, 0} -> false;
            {ok, 1} -> true;
            {error, _Err} -> error(_Err)
        end).

-define(term(_Val),
        case _Val of
            {ok, _Term} -> _Term;
            {error, _Err} -> error(_Err)
        end).

-define(maybe_term(_Result),
    case _Result of
        {ok, _Value} -> {ok, _Value};
        undefined -> undefined;
        {error, _Err} -> error(_Err)
    end).

%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Connect to a locally running Redis server.
%% @spec connect() -> {ok, Client}  | {error, Reason}
%% @equiv connect([])
%% @end
%%--------------------------------------------------------------------

connect() ->
    connect([]).

%%--------------------------------------------------------------------
%% @doc Connect to a host running Redis using the specified options.
%% @spec connect(Options) -> {ok, Client} | {error, Reason}
%% Options = [connect_option()]
%% connect_option() = {host, Host} |
%%                    {port, Port} |
%%                    {recipient, Recipient}
%% Host = string()
%% Port = port()
%% Recipient = function() | MFA | pid()
%% MFA = {atom(), atom(), [term()]}
%% Client = client()
%% @end
%%--------------------------------------------------------------------

connect(Options) ->
    redis_client:start_link(Options).

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
    ?maybe_term(redis_client:request(Client, {"GET", [Key]})).

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

%%--------------------------------------------------------------------
%% @doc Rewrites the append-only file to reflect the current dataset
%% in memory.
%%
%% Redis command: [http://redis.io/commands/bgrewriteaof BGREWRITEAOF]
%%
%% @spec bgrewriteaof(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

bgrewriteaof(Client) ->
    ?ok(redis_client:request(Client, {"BGREWRITEAOF", []})).

%%--------------------------------------------------------------------
%% @doc Save the DB in background.
%%
%% Redis forks, the parent continues to server the clients, the child
%% saves the DB on disk then exit. A client my be able to check if
%% the operation succeeded using `lastsave/1'.
%%
%% Redis command: [http://redis.io/commands/bgsave BGSAVE]
%%
%% @spec bgsave(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

bgsave(Client) ->
    ?ok(redis_client:request(Client, {"BGSAVE", []})).

%%--------------------------------------------------------------------
%% @doc Return the UNIX TIME of the last DB save executed with success.
%%
%% Redis command: [http://redis.io/commands/lastsave LASTSAVE]
%%
%% @spec lastsave(Client) -> integer()
%% Client = client()
%% @end
%%--------------------------------------------------------------------

lastsave(Client) ->
    ?term(redis_client:request(Client, {"LASTSAVE", []})).

%%--------------------------------------------------------------------
%% @doc Insert a value at the head of a list.
%%
%% Returns the length of the list after the push.
%%
%% Redis command: [http://redis.io/commands/lpush LPUSH]
%%
%% @spec lpush(Client, Key, Value) -> integer()
%% Client = client()
%% Key = key()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

lpush(Client, Key, Value) ->
    ?term(redis_client:request(Client, {"LPUSH", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Insert a value at the head of a list only if Key exists.
%%
%% Returns the length of the list after the push.
%%
%% Redis command: [http://redis.io/commands/lpushx LPUSHX]
%%
%% @spec lpushx(Client, Key, Value) -> integer()
%% Client = client()
%% Key = key()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

lpushx(Client, Key, Value) ->
    ?term(redis_client:request(Client, {"LPUSHX", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Inserts multiple values at the head of a list.
%%
%% Returns the length of the list after the push.
%%
%% Redis command: [http://redis.io/commands/lpush LPUSH]
%%
%% @spec mlpush(Client, Key, Values) -> integer()
%% Client = client()
%% Key = key()
%% Values = [value()]
%% @end
%%--------------------------------------------------------------------

mlpush(Client, Key, Values) ->
    ?term(redis_client:request(Client, {"LPUSH", [Key|Values]})).

%%--------------------------------------------------------------------
%% @doc Removes and returns the first element of a list.
%%
%% Redis command: [http://redis.io/commands/lpop LPOP]
%%
%% @spec lpop(Client, Key) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

lpop(Client, Key) ->
    ?maybe_term(redis_client:request(Client, {"LPOP", [Key]})).

%%--------------------------------------------------------------------
%% @doc Blocking version of `lpop'.
%%
%% Redis command: [http://redis.io/commands/blpop BLPOP]
%%
%% @spec blpop(Client, Key, Timeout) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Timeout = integer()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

blpop(Client, Key, Timeout) ->
    ?maybe_term(redis_client:request(
                  Client, {"BLPOP", [Key, Timeout]},
                  request_timeout(Timeout))).

%%--------------------------------------------------------------------
%% @doc Version of `mlpop' that supports multiple keys. Keys are
%% checked in the order given.
%%
%% Redis command: [http://redis.io/commands/blpop BLPOP]
%%
%% @spec bmlpop(Client, Keys, Timeout) -> {ok, Value} | undefined
%% Client = client()
%% Keys = [key()]
%% Timeout = integer()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

bmlpop(Client, Keys, Timeout) ->
    ?maybe_term(redis_client:request(
                  Client, {"BLPOP", lits:append(Keys, [Timeout])},
                  request_timeout(Timeout))).

%%--------------------------------------------------------------------
%% @doc Returns the value at an indexed location in a list or undefined
%% if the item isn't in the list.
%%
%% Index starts from the left at position 0.
%%
%% Redis command: [http://redis.io/commands/lindex LINDEX]
%%
%% @spec lindex(Client, Key, Index) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Index = integer()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

lindex(Client, Key, Index) ->
    ?maybe_term(redis_client:request(Client, {"LINDEX", [Key, Index]})).

%%--------------------------------------------------------------------
%% @doc Inserts a value before or after a reference value pivot.
%%
%% Returns the length of the list after the operation or false if pivot
%% does not exist.
%%
%% Redis command: [http://redis.io/commands/linsert LINSERT]
%%
%% @spec linsert(Client, Key, Where, Pivot, Value) -> integer() | false
%% Client = client()
%% Key = key()
%% Where = before | after
%% Pivot = value()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

linsert(Client, Key, Where, Pivot, Value) ->
    case redis_client:request(
           Client, {"LINSERT", [Key, where_arg(Where), Pivot, Value]}) of
        {ok, -1} -> false;
        {ok, Count} -> Count;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc Get the length of a list.
%%
%% Redis command: [http://redis.io/commands/llen LLEN]
%%
%% @spec llen(Client, Key) -> integer()
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

llen(Client, Key) ->
    ?term(redis_client:request(Client, {"LLEN", [Key]})).

%%--------------------------------------------------------------------
%% @doc Returns the specified elements of the list at Key.
%%
%% Redis command: [http://redis.io/commands/lrange LRANGE]
%%
%% @spec lrange(Client, Key, Start, Stop) -> Values
%% @end
%%--------------------------------------------------------------------

lrange(Client, Key, Start, Stop) ->
    ?term(redis_client:request(Client, {"LRANGE", [Key, Start, Stop]})).

%%--------------------------------------------------------------------
%% @doc Removes the first Count elements equal to Value from a list.
%%
%% Returns the number of elements removed from the list.
%%
%% Redis command: [http://redis.io/commands/lrem LREM]
%%
%% @spec lrem(Client, Key, Count, Value) -> integer()
%% Client = client()
%% Key = key()
%% Count = integer()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

lrem(Client, Key, Count, Value) ->
    ?term(redis_client:request(Client, {"LREM", [Key, Count, Value]})).

%%--------------------------------------------------------------------
%% @doc Sets the list element at index to value.
%%
%% Redis command: [http://redis.io/commands/lset LSET]
%%
%% @spec lset(Client, Key, Index, Value) -> ok
%% Client = client()
%% Key = key()
%% Index = integer()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

lset(Client, Key, Index, Value) ->
    ?ok(redis_client:request(Client, {"LSET", [Key, Index, Value]})).

%%--------------------------------------------------------------------
%% @doc Trim an existing list to cointain only the specified range.
%%
%% Redis command: [http://redis.io/commands/ltrim LTRIM]
%%
%% @spec ltrim(Client, Key, Start, Stop) -> ok
%% @end
%%--------------------------------------------------------------------

ltrim(Client, Key, Start, Stop) ->
    ?ok(redis_client:request(Client, {"LTRIM", [Key, Start, Stop]})).

%%--------------------------------------------------------------------
%% @doc Insert a value at the end of a list.
%%
%% Returns the length of the list after the push.
%%
%% Redis command: [http://redis.io/commands/rpush RPUSH]
%%
%% @spec rpush(Client, Key, Value) -> integer()
%% Client = client()
%% Key = key()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

rpush(Client, Key, Value) ->
    ?term(redis_client:request(Client, {"RPUSH", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Insert a value at the end of a list only if Key exists.
%%
%% Returns the length of the list after the push.
%%
%% Redis command: [http://redis.io/commands/rpushx RPUSHX]
%%
%% @spec rpushx(Client, Key, Value) -> integer()
%% Client = client()
%% Key = key()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

rpushx(Client, Key, Value) ->
    ?term(redis_client:request(Client, {"RPUSHX", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Inserts multiple values at the end of a list.
%%
%% Returns the length of the list after the push.
%%
%% Redis command: [http://redis.io/commands/rpush RPUSH]
%%
%% @spec mrpush(Client, Key, Values) -> integer()
%% Client = client()
%% Key = key()
%% Values = [value()]
%% @end
%%--------------------------------------------------------------------

mrpush(Client, Key, Values) ->
    ?term(redis_client:request(Client, {"RPUSH", [Key|Values]})).

%%--------------------------------------------------------------------
%% @doc Removes and returns the last element of a list.
%%
%% Redis command: [http://redis.io/commands/rpop RPOP]
%%
%% @spec rpop(Client, Key) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

rpop(Client, Key) ->
    ?maybe_term(redis_client:request(Client, {"RPOP", [Key]})).

%%--------------------------------------------------------------------
%% @doc Blocking version of `rpop'.
%%
%% Redis command: [http://redis.io/commands/brpop BRPOP]
%%
%% @spec brpop(Client, Key, Timeout) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Timeout = integer()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

brpop(Client, Key, Timeout) ->
    ?maybe_term(redis_client:request(
                  Client, {"BRPOP", [Key, Timeout]},
                  request_timeout(Timeout))).

%%--------------------------------------------------------------------
%% @doc Version of `mrpop' that supports multiple keys. Keys are
%% checked in the order given.
%%
%% Redis command: [http://redis.io/commands/brpop BRPOP]
%%
%% @spec bmrpop(Client, Keys, Timeout) -> {ok, Value} | undefined
%% Client = client()
%% Keys = [key()]
%% Timeout = integer()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

bmrpop(Client, Keys, Timeout) ->
    ?maybe_term(redis_client:request(
                  Client, {"BRPOP", lists:append(Keys, [Timeout])},
                  request_timeout(Timeout))).

%%--------------------------------------------------------------------
%% @doc The blocking variant of rpoplpush/3.
%%
%% Timeout is in seconds and may be 0 to block indefinitely.
%%
%% Redis command: [http://redis.io/commands/brpoplpush BRPOPLPUSH]
%%
%% @spec brpoplpush(Client, Source, Destination, Timeout) ->
%%                                               undefined | {ok, Value}
%% Client = client()
%% Source = key()
%% Destination = key()
%% Timeout = integer()
%% Value = store_value()
%% @end
%%--------------------------------------------------------------------

brpoplpush(Client, Source, Destination, Timeout) ->
    ?maybe_term(redis_client:request(
                  Client, {"BRPOPLPUSH", [Source, Destination, Timeout]})).

%%--------------------------------------------------------------------
%% @doc Atomically returns and removes the last element of the list at
%% Source and pushes the element at the first element at Destination.
%%
%% Redis command: [http://redis.io/commands/rpoplpush RPOPLPUSH]
%%
%% @spec rpoplpush(Client, Source, Destination) -> undefined | {ok, Value}
%% Client = client()
%% Source = key()
%% Destination = key()
%% Value = store_value()
%% @end
%%--------------------------------------------------------------------

rpoplpush(Client, Source, Destination) ->
    ?maybe_term(redis_client:request(
                  Client, {"RPOPLPUSH", [Source, Destination]})).

%%--------------------------------------------------------------------
%% @doc Returns configuration parameters of the server.
%%
%% Parameter can be a glob expression to match multiple parameters.
%%
%% Redis command: [http://redis.io/commands/config-get CONFIG GET]
%%
%% @spec config_get(Client, Parameter) ->  paramlist()
%% Client = client()
%% Parameter = string()
%% paramlist() = [{string(), term()}]
%% @end
%%--------------------------------------------------------------------

config_get(Client, Parameter) ->
    multi_proplist(?term(redis_client:request(
                           Client, {"CONFIG", ["GET", Parameter]}))).

%%--------------------------------------------------------------------
%% @doc Reconfigures the server without the need to restart it.
%%
%% Redis command: [http://redis.io/commands/config-set CONFIG SET]
%%
%% @spec config_set(Client, Parameter, Value) -> ok
%% Client = client()
%% Parameter = string()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

config_set(Client, Parameter, Value) ->
    ?ok(redis_client:request(Client, {"CONFIG", ["SET", Parameter, Value]})).

%%--------------------------------------------------------------------
%% @doc Resets the statistics reported by info/1.
%%
%% Redis command: [http://redis.io/commands/config-resetstat CONFIG RESETSTAT]
%%
%% @spec config_resetstat(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

config_resetstat(Client) ->
    ?ok(redis_client:request(Client, {"CONFIG", ["RESETSTAT"]})).

%%--------------------------------------------------------------------
%% @doc Get information about an object.
%%
%% Returns error if the key doesn't exist.
%%
%% Redis command: [http://redis.io/commands/debug-object DEBUG OBJECT]
%%
%% @spec debug_object(Client, Key) -> {ok, Info} | error
%% Client = client()
%% Key = key()
%% Info = string()
%% @end
%%--------------------------------------------------------------------

debug_object(Client, Key) ->
    case redis_client:request(Client, {"DEBUG", ["OBJECT", Key]}) of
        {ok, Info} -> {ok, Info};
        {error, "no such key"} -> error;
        {error, Err} -> error(Err)
    end.

%%--------------------------------------------------------------------
%% @doc This function is not implemented but is here for completeness.
%%
%% Redis command: [http://redis.io/commands/debug-segfault DEBUG SEGFAULT]
%%
%% @spec debug_segfault(Client) -> any()
%% Client = client()
%% @end
%%--------------------------------------------------------------------

debug_segfault(_Client) ->
    error(not_implemented).

%%--------------------------------------------------------------------
%% @doc Flushes all previously queued commands in a transaction and
%% restores the connection state to normal.
%%
%% Redis command: [http://redis.io/commands/discard DISCARD]
%%
%% @spec discard(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

discard(Client) ->
    ?ok(redis_client:request(Client, {"DISCARD", []})).

%%--------------------------------------------------------------------
%% @doc Executes all previously queued commands in a transaction and
%% restores the connection state to normal.
%%
%% Redis command: [http://redis.io/commands/exec EXEC]
%%
%% @spec exec(Client) -> ok
%% @end
%%--------------------------------------------------------------------

exec(Client) ->
    ?ok(redis_client:request(Client, {"EXEC", []})).

%%--------------------------------------------------------------------
%% @doc Set a timeout on key.
%%
%% After the timeout has expired, the key will automatically be deleted.
%%
%% Returns true if the timeout was set, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/expire EXPIRE]
%%
%% @spec expire(Client, Key, Seconds) -> boolean()
%% Client = client()
%% Key = key()
%% Seconds = integer()
%% @end
%%--------------------------------------------------------------------

expire(Client, Key, Seconds) ->
    ?bool(redis_client:request(Client, {"EXPIRE", [Key, Seconds]})).

%%--------------------------------------------------------------------
%% @doc Set a timeout on key in epoch seconds.
%%
%% Returns true if the timeout was set successfully, otherwise returns
%% false.
%%
%% Redis command: [http://redis.io/commands/expireat EXPIREAT]
%%
%% @spec expireat(Client, Key, Timeout) -> boolean()
%% Client = client()
%% Key = key()
%% Timeout = integer()
%% @end
%%--------------------------------------------------------------------

expireat(Client, Key, Timeout) ->
    ?bool(redis_client:request(Client, {"EXPIREAT", [Key, Timeout]})).

%%--------------------------------------------------------------------
%% @doc Delete all the keys of all the existing databases.
%%
%% Redis command: [http://redis.io/commands/flushall FLUSHALL]
%%
%% @spec flushall(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

flushall(Client) ->
    ?ok(redis_client:request(Client, {"FLUSHALL", []})).

%%--------------------------------------------------------------------
%% @doc Returns the bit value at offset in the string value stored at key.
%%
%% Redis command: [http://redis.io/commands/getbit GETBIT]
%%
%% @spec getbit(Client) -> 0 | 1
%% @end
%%--------------------------------------------------------------------

getbit(Client, Key, Offset) ->
    ?term(redis_client:request(Client, {"GETBIT", [Key, Offset]})).

%%--------------------------------------------------------------------
%% @doc Returns a substring of the string value stored at Key.
%%
%% Redis command: [http://redis.io/commands/getrange GETRANGE]
%%
%% @spec getrange(Client, Key, Start, End) -> string()
%% Client = client()
%% Key = key()
%% Start = start()
%% End = end()
%% @end
%%--------------------------------------------------------------------

getrange(Client, Key, Start, End) ->
    ?term(redis_client:request(Client, {"GETRANGE", [Key, Start, End]})).

%%--------------------------------------------------------------------
%% @doc Atomically sets key to value and returns the old value stored
%% at key.
%%
%% Redis command: [http://redis.io/commands/getset GETSET]
%%
%% @spec getset(Client, Key, Value) -> {ok, OldValue} | undefined
%% Client = client()
%% Key = key()
%% Value = value()
%% OldValue = stored_value()
%% @end
%%--------------------------------------------------------------------

getset(Client, Key, Value) ->
    ?maybe_term(redis_client:request(Client, {"GETSET", [Key, Value]})).

%%--------------------------------------------------------------------
%% @doc Removes the specified fields from the hash stored at Key.
%%
%% Redis command: [http://redis.io/commands/hdel HDEL]
%%
%% @spec hdel(Client, Key, Fields) -> integer()
%% Client = client()
%% Key = key()
%% Fields = [field()]
%% @end
%%--------------------------------------------------------------------

hdel(Client, Key, Fields) ->
    ?term(redis_client:request(Client, {"HDEL", [Key|Fields]})).

%%--------------------------------------------------------------------
%% @doc Returns if Field is an existing field in the hash stored at Key.
%%
%% Redis command: [http://redis.io/commands/hexists HEXISTS]
%%
%% @spec hexists(Client, Key, Field) -> boolean()
%% Client = client()
%% Key = key()
%% Field = field()
%% @end
%%--------------------------------------------------------------------

hexists(Client, Key, Field) ->
    ?bool(redis_client:request(Client, {"HEXISTS", [Key, Field]})).

%%--------------------------------------------------------------------
%% @doc Returns the value associated with Field in the hash stored at Key.
%%
%% Redis command: [http://redis.io/commands/hget HGET]
%%
%% @spec hget(Client, Key, Field) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Field = field()
%% @end
%%--------------------------------------------------------------------

hget(Client, Key, Field) ->
    ?maybe_term(redis_client:request(Client, {"HGET", [Key, Field]})).

%%--------------------------------------------------------------------
%% @doc Returns all fields and values of the hash stored at Key.
%%
%% Redis command: [http://redis.io/commands/hgetall HGETALL]
%%
%% @spec hgetall(Client, Key) -> [{stored_field(), stored_value()}]
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

hgetall(Client, Key) ->
    multi_hashlist(?term(redis_client:request(Client, {"HGETALL", [Key]}))).

%%--------------------------------------------------------------------
%% @doc Increments the number stored at Field in the hash stored at Key
%% by increment.
%%
%% Redis command: [http://redis.io/commands/hincrby HINCRBY]
%%
%% @spec hincrby(Client, Key, Field, Increment) -> integer()
%% Client = client()
%% Key = key()
%% Field = field()
%% Increment = integer()
%% @end
%%--------------------------------------------------------------------

hincrby(Client, Key, Field, Increment) ->
    ?term(redis_client:request(Client, {"HINCRBY", [Key, Field, Increment]})).

%%--------------------------------------------------------------------
%% @doc Returns all field names in the hash stored at Key.
%%
%% Redis command: [http://redis.io/commands/hkeys HKEYS]
%%
%% @spec hkeys(Client, Key) -> [stored_field()]
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

hkeys(Client, Key) ->
    ?term(redis_client:request(Client, {"HKEYS", [Key]})).

%%--------------------------------------------------------------------
%% @doc Returns the number of fields contained in the hash stored at Key.
%%
%% Redis command: [http://redis.io/commands/hlen HLEN]
%%
%% @spec hlen(Client, Key) -> integer()
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

hlen(Client, Key) ->
    ?term(redis_client:request(Client, {"HLEN", [Key]})).

%%--------------------------------------------------------------------
%% @doc Returns the values associated with Fields in the hash stored at Key.
%%
%% Fields must contain at least one element.
%%
%% Redis command: [http://redis.io/commands/hmget HMGET]
%%
%% @spec hmget(Client, Key, Fields) -> [stored_value() | undefined]
%% Client = client()
%% Key = key()
%% Fields = [field()]
%% @end
%%--------------------------------------------------------------------

hmget(Client, Key, Fields) when length(Fields) > 0 ->
    ?maybe_term(redis_client:request(Client, {"HMGET", [Key|Fields]})).

%%--------------------------------------------------------------------
%% @doc Sets the specified fields to their respective values in the
%% hash stored at Key.
%%
%% FieldValues must contain at least one element.
%%
%% Redis command: [http://redis.io/commands/hmset HMSET]
%%
%% @spec hmset(Client, Key, FieldValues) -> ok
%% Client = client()
%% Key = key()
%% FieldValues = [{field(), value()}]
%% @end
%%--------------------------------------------------------------------

hmset(Client, Key, FieldValues) when length(FieldValues) > 0 ->
    ?ok(redis_client:request(
          Client, {"HMSET", [Key|field_value_args(FieldValues)]})).

%%--------------------------------------------------------------------
%% @doc Sets field in the hash stored at key to value.
%%
%% Redis command: [http://redis.io/commands/hset HSET]
%%
%% @spec hset(Client, Key, Field, Value) -> boolean()
%% Client = client()
%% Key = key()
%% Field = field()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

hset(Client, Key, Field, Value) ->
    ?bool(redis_client:request(Client, {"HSET", [Key, Field, Value]})).

%%--------------------------------------------------------------------
%% @doc Sets Field in the hash stored at Key to Value, only if Field
%% does not yet exist.
%%
%% Redis command: [http://redis.io/commands/hsetnx HSETNX]
%%
%% @spec hsetnx(Client, Key, Field, Value) -> boolean()
%% Client = client()
%% Key = key()
%% Field = field()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

hsetnx(Client, Key, Field, Value) ->
    ?bool(redis_client:request(Client, {"HSETNX", [Key, Field, Value]})).

%%--------------------------------------------------------------------
%% @doc Returns all values in the hash stored at Key.
%%
%% Redis command: [http://redis.io/commands/hvals HVALS]
%%
%% @spec hvals(Client, Key) -> [stored_value()]
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

hvals(Client, Key) ->
    ?term(redis_client:request(Client, {"HVALS", [Key]})).

%%--------------------------------------------------------------------
%% @doc Get information and stantistics about the server.
%%
%% Redis command: [http://redis.io/commands/info INFO]
%%
%% @spec info(Client) -> proplist()
%% Client = client()
%% @end
%%--------------------------------------------------------------------

info(Client) ->
    bulk_proplist(?term(redis_client:request(Client, {"INFO", []}))).

%%--------------------------------------------------------------------
%% @doc Returns the values of all specified keys.
%%
%% Keys must contain at least one element.
%%
%% Redis command: [http://redis.io/commands/mget MGET]
%%
%% @spec mget(Client, Keys) -> [stored_value()]
%% Client = client()
%% Keys = [key()]
%% @end
%%--------------------------------------------------------------------

mget(Client, Keys) when length(Keys) > 0 ->
    ?term(redis_client:request(Client, {"MGET", Keys})).

%%--------------------------------------------------------------------
%% @doc This function is not implemented but is listed here for
%% completeness.
%%
%% Use a telnet session to monitor the server.
%%
%% Redis command: [http://redis.io/commands/monitor MONITOR]
%%
%% @spec monitor(Client) -> any()
%% @end
%%--------------------------------------------------------------------

monitor(_Client) ->
    error({not_implemented, "use a telnet session to monitor a server"}).

%%--------------------------------------------------------------------
%% @doc Move Key from the currently selected database to the specified
%% destination database.
%%
%% Redis command: [http://redis.io/commands/move MOVE]
%%
%% @spec move(Client, Key, Destination) -> boolean()
%% Client = client()
%% Key = key()
%% Destination = integer()
%% @end
%%--------------------------------------------------------------------

move(Client, Key, Destination) when is_integer(Destination) ->
    ?bool(redis_client:request(Client, {"MOVE", [Key, Destination]})).

%%--------------------------------------------------------------------
%% @doc Sets the given keys to their respective values.
%%
%% KeyValues must contain at least one element.
%%
%% Redis command: [http://redis.io/commands/mset MSET]
%%
%% @spec mset(Client, KeyValues) -> ok
%% Client = client()
%% KeyValues = [{key(), value()}]
%% @end
%%--------------------------------------------------------------------

mset(Client, KeyValues) ->
    ?ok(redis_client:request(Client, {"MSET", key_value_args(KeyValues)})).

%%--------------------------------------------------------------------
%% @doc Sets the given keys to their respective values only if none if
%% specified keys exist.
%%
%% KeyValues must contain at least one element.
%%
%% Redis command: [http://redis.io/commands/msetnx MSETNX]
%%
%% @spec mset(Client, KeyValues) -> boolean
%% Client = client()
%% KeyValues = [{key(), value()}]
%% @end
%%--------------------------------------------------------------------

msetnx(Client, KeyValues) ->
    ?bool(redis_client:request(
            Client, {"MSETNX", key_value_args(KeyValues)})).

%%--------------------------------------------------------------------
%% @doc Marks the start of a transaction block.
%%
% Subsequent commands will be queued for atomic execution using EXEC.
%%
%% Redis command: [http://redis.io/commands/multi MULTI]
%%
%% @spec multi(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

multi(Client) ->
    ?ok(redis_client:request(Client, {"MULTI", []})).

%%--------------------------------------------------------------------
%% @doc Returns information about an object at Key.
%%
%% Redis command: [http://redis.io/commands/object OBJECT]
%%
%% @spec object(Client, Info, Key) -> {ok, Value} | undefined
%% Client = client()
%% Info = refcount | encoding | idletime
%% Key = key()
%% @end
%%--------------------------------------------------------------------

object(Client, Info, Key) ->
    ?maybe_term(redis_client:request(
                  Client, {"OBJECT", [object_subcommand(Info), Key]})).

%%--------------------------------------------------------------------
%% @doc Remove the existing timeout on key.
%%
%% Returns true if the timeout was removed, otherwise returns false.
%%
%% Redis command: [http://redis.io/commands/persist PERSIST]
%%
%% @spec persist(Client, Key) -> boolean()
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

persist(Client, Key) ->
    ?bool(redis_client:request(Client, {"PERSIST", [Key]})).

%%--------------------------------------------------------------------
%% @doc Subscribes the client to the given patterns.
%%
%% Redis command: [http://redis.io/commands/psubscribe PSUBSCRIBE]
%%
%% @spec psubscribe(Client, Patterns) -> ok
%% Client = client()
%% Patterns = [pattern()]
%% @end
%%--------------------------------------------------------------------

psubscribe(Client, Patterns) ->
    ?ok(redis_client:request(Client, {"PSUBSCRIBE", Patterns})).

%%--------------------------------------------------------------------
%% @doc Posts a message to the given channel.
%%
%% Redis command: [http://redis.io/commands/publish PUBLISH]
%%
%% Returns the number of clients that received the message.
%%
%% @spec publish(Client, Channel, Message) -> integer()
%% Client = client()
%% Channel = channel()
%% Message = value()
%% @end
%%--------------------------------------------------------------------

publish(Client, Channel, Message) ->
    ?term(redis_client:request(Client, {"PUBLISH", [Channel, Message]})).

%%--------------------------------------------------------------------
%% @doc Unsubscribes the client from all patterns.
%%
%% Redis command: [http://redis.io/commands/punsubscribe PUNSUBSCRIBE]
%%
%% @spec punsubscribe(Client, Patterns) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

punsubscribe(Client) ->
    ?ok(redis_client:request(Client, {"PUNSUBSCRIBE", []})).

%%--------------------------------------------------------------------
%% @doc Unsubscribes the client from the given patterns.
%%
%% Redis command: [http://redis.io/commands/punsubscribe PUNSUBSCRIBE]
%%
%% @spec punsubscribe(Client, Patterns) -> ok
%% Client = client()
%% Patterns = [pattern()]
%% @end
%%--------------------------------------------------------------------

punsubscribe(Client, Patterns) ->
    ?ok(redis_client:request(Client, {"PUNSUBSCRIBE", Patterns})).

%%--------------------------------------------------------------------
%% @doc Return a random key from the currently selected database.
%%
%% Redis command: [http://redis.io/commands/randomkey RANDOMKEY]
%%
%% @spec randomkey(Client) -> stored_key()
%% Client = client()
%% @end
%%--------------------------------------------------------------------

randomkey(Client) ->
    ?term(redis_client:request(Client, {"RANDOMKEY", []})).

%%--------------------------------------------------------------------
%% @doc Not sure what this does - not implemented.
%%
%% Redis command: [http://redis.io/commands/save SAVE]
%%
%% @spec save(Client) -> any()
%% @end
%%--------------------------------------------------------------------

save(_Client) ->
    error(not_implemented).

%%--------------------------------------------------------------------
%% @doc Returns the number of elements of the set stored at Key.
%%
%% Returns 0 if Key doesn't exist.
%%
%% Redis command: [http://redis.io/commands/scard SCARD]
%%
%% @spec scard(Client, Key) -> integer()
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

scard(Client, Key) ->
    ?term(redis_client:request(Client, {"SCARD", [Key]})).

%%--------------------------------------------------------------------
%% @doc Returns the members of the set resulting from the difference
%% between the first set and all the successive sets.
%%
%% Keys must contain at least two keys.
%%
%% Redis command: [http://redis.io/commands/sdiff SDIFF]
%%
%% @spec sdiff(Client, SetKeys) -> Diff
%% Client = client()
%% SetKeys = [key()]
%% Diff = [stored_value()]
%% @end
%%--------------------------------------------------------------------

sdiff(Client, SetKeys) when length(SetKeys) > 1 ->
    ?term(redis_client:request(Client, {"SDIFF", SetKeys})).

%%--------------------------------------------------------------------
%% @doc Store the result of `sdiff' in Destination rather than return it.
%%
%% Redis command: [http://redis.io/commands/sdiffstore SDIFFSTORE]
%%
%% @spec sdiffstore(Client, Destination, SetKeys) -> integer()
%% Client = client()
%% Destination = key()
%% SetKeys = [key()]
%% @end
%%--------------------------------------------------------------------

sdiffstore(Client, Destination, SetKeys) when length(SetKeys) >  1 ->
    ?term(redis_client:request(
            Client, {"SDIFFSTORE", [Destination|SetKeys]})).

%%--------------------------------------------------------------------
%% @doc Select the DB with having the specified zero-based numeric index.
%%
%% Redis command: [http://redis.io/commands/select SELECT]
%%
%% @spec select(Client, DB) -> ok
%% Client = client()
%% DB = integer()
%% @end
%%--------------------------------------------------------------------

select(Client, DB) when is_integer(DB) ->
    ?ok(redis_client:request(Client, {"SELECT", [DB]})).

%%--------------------------------------------------------------------
%% @doc Sets or clears the bit at offset in the string value stored at key.
%%
%% Returns the original bit value.
%%
%% Redis command: [http://redis.io/commands/setbit SETBIT]
%%
%% @spec setbit(Client, Key, Offset, Value) -> integer()
%% Client = client()
%% Key = key()
%% Offset = integer()
%% Value = 0 | 1
%% @end
%%--------------------------------------------------------------------

setbit(Client, Key, Offset, Value) ->
    ?term(redis_client:request(Client, {"SETBIT", [Key, Offset, Value]})).

%%--------------------------------------------------------------------
%% @doc Set a string value that will expire after a number of seconds.
%%
%% Redis command: [http://redis.io/commands/setex SETEX]
%%
%% @spec setex(Client, Key, Seconds, Value) -> ok
%% Client = client()
%% Key = key()
%% Seconds = integer()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

setex(Client, Key, Seconds, Value) ->
    ?ok(redis_client:request(Client, {"SETEX", [Key, Seconds, Value]})).

%%--------------------------------------------------------------------
%% @doc Overwrites part of the string stored at key, starting at the
%% specified offset, for the entire length of value.
%%
%% Redis command: [http://redis.io/commands/setrange SETRANGE]
%%
%% @spec setrange(Client, Key, Offset, Value) -> integer()
%% Client = client()
%% Key = key()
%% Offset = integer()
%% Value = value()
%% @end
%%--------------------------------------------------------------------

setrange(Client, Key, Offset, Value) ->
    ?term(redis_client:request(Client, {"SETRANGE", [Key, Offset, Value]})).

%%--------------------------------------------------------------------
%% @doc Shut down the server.
%%
%% This function will return immediately with ok. Because the server
%% disconnects all clients on shutdown, the client will exit shortly
%% after this call.
%%
%% Redis command: [http://redis.io/commands/shutdown SHUTDOWN]
%%
%% @spec shutdown(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

shutdown(Client) ->
    try
        redis_client:request(Client, {"SHUTDOWN", []}, 0)
    catch
        exit:{timeout, _} -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Returns the members of the set resulting from the intersection
%% of all the given sets.
%%
%% Redis command: [http://redis.io/commands/sinter SINTER]
%%
%% @spec sinter(Client, SetKeys) -> Intersection
%% Client = client()
%% SetKeys = [key()]
%% Intersection = [stored_value()]
%% @end
%%--------------------------------------------------------------------

sinter(Client, SetKeys) when length(SetKeys) > 1 ->
    ?term(redis_client:request(Client, {"SINTER", SetKeys})).

%%--------------------------------------------------------------------
%% @doc Same as `setinter' but stores intersection in Destination.
%%
%% Redis command: [http://redis.io/commands/sinterstore SINTERSTORE]
%%
%% @spec sinterstore(Client, Destination, SetKeys) -> integer()
%% Client = client()
%% Destination = key()
%% SetKeys = [key()]
%% @end
%%--------------------------------------------------------------------

sinterstore(Client, Destination, SetKeys) ->
    ?term(redis_client:request(
            Client, {"SINTERSTORE", [Destination|SetKeys]})).

%%--------------------------------------------------------------------
%% @doc Configures the server's slave settings.
%%
%% Redis command: [http://redis.io/commands/slaveof SLAVEOF]
%%
%% @spec slaveof(Client, SlaveSettings) -> ok
%% SlaveSettings = master() | no_one
%% master() = Host | {Host, Port}
%% Host = string()
%% Port = port()
%% @end
%%--------------------------------------------------------------------

slaveof(Client, no_one) ->
    ?ok(redis_client:request(Client, {"SLAVEOF", ["NO", "ONE"]}));
slaveof(Client, {Host, Port}) ->
    ?ok(redis_client:request(Client, {"SLAVEOF", [Host, Port]}));
slaveof(Client, Host) ->
    ?ok(redis_client:request(Client, {"SLAVEOF", [Host, ?DEFAULT_PORT]})).

%%--------------------------------------------------------------------
%% @doc Queries or resets the slow log.
%%
%% Redis command: [http://redis.io/commands/slowlog SLOWLOG]
%%
%% @spec slowlog(Client, Command) -> Result
%% Command = get | {get, N} | len | reset
%% Result = [string()] | integer() | ok
%% @end
%%--------------------------------------------------------------------

slowlog(Client, get) ->
    ?term(redis_client:request(Client, {"SLOWLOG", ["GET"]}));
slowlog(Client, {get, N}) when is_integer(N), N > 0 ->
    ?term(redis_client:request(Client, {"SLOWLOG", ["GET", N]}));
slowlog(Client, len)->
    ?term(redis_client:request(Client, {"SLOWLOG", ["LEN"]}));
slowlog(Client, reset) ->
    ?ok(redis_client:request(Client, {"SLOWLOG", ["RESET"]})).

%%--------------------------------------------------------------------
%% @doc Move member from the set at source to the set at destination.
%%
%% Redis command: [http://redis.io/commands/smove SMOVE]
%%
%% @spec smove(Client, Source, Destination, Member) -> boolean()
%% Client = client()
%% Source = key()
%% Destination = key()
%% Member = value()
%% @end
%%--------------------------------------------------------------------

smove(Client, Source, Destination, Member) ->
    ?bool(redis_client:request(
            Client, {"SMOVE", [Source, Destination, Member]})).

%%--------------------------------------------------------------------
%% @doc Sorts the values at Key.
%%
%% Redis command: [http://redis.io/commands/sort SORT]
%%
%% @spec sort(Client, Key) -> Result
%% @equiv sort(Client, Key, [])
%% @end
%%--------------------------------------------------------------------

sort(Client, Key) ->
    sort(Client, Key, []).

%%--------------------------------------------------------------------
%% @doc Sorts the values at Key using Options.
%%
%% Multiple get options can be provided using {mget, [get()]}. If
%% multiple gets are specified, the result contains a list of lists,
%% each list containing an item corresponding to a get.
%%
%% Redis command: [http://redis.io/commands/sort SORT]
%%
%% @spec sort(Client, Key, Options) -> Result
%% Client = client()
%% Key = key()
%% Options = [sort_option()]
%% sort_option() = {limit, Offset, Count} |
%%                 asc | desc | alpha |
%%                 {by, By} | {get, Get}, {mget, Gets} |
%%                 {store, Store} |
%%                 nosort
%% Result = [stored_value() | [stored_value()]]
%% @end
%%--------------------------------------------------------------------

sort(Client, Key, Options) ->
    sort_result(
      ?term(redis_client:request(Client, {"SORT", [Key|sort_args(Options)]})),
      Options).

%%--------------------------------------------------------------------
%% @doc Removes and returns a random element from the set value
%% stored at Key.
%%
%% Redis command: [http://redis.io/commands/spop SPOP]
%%
%% @spec spop(Client, Key) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

spop(Client, Key) ->
    ?maybe_term(redis_client:request(Client, {"SPOP", [Key]})).

%%--------------------------------------------------------------------
%% @doc Return a random element from the set value stored at key.
%%
%% Redis command: [http://redis.io/commands/srandmember SRANDMEMBER]
%%
%% @spec srandmember(Client, Key) -> {ok, Value} | undefined
%% Client = client()
%% Key = key()
%% Value = stored_value()
%% @end
%%--------------------------------------------------------------------

srandmember(Client, Key) ->
    ?maybe_term(redis_client:request(Client, {"SRANDMEMBER", [Key]})).

%%--------------------------------------------------------------------
%% @doc Returns the length of the string value stored at Key.
%%
%% Redis command: [http://redis.io/commands/strlen STRLEN]
%%
%% @spec strlen(Client, Key) -> integer()
%% @end
%%--------------------------------------------------------------------

strlen(Client, Key) ->
    ?term(redis_client:request(Client, {"STRLEN", [Key]})).

%%--------------------------------------------------------------------
%% @doc Subscribes the client to the specified channels.
%%
%% Redis command: [http://redis.io/commands/subscribe SUBSCRIBE]
%%
%% @spec subscribe(Client, Channels) -> ok
%% @end
%%--------------------------------------------------------------------

subscribe(Client, Channels) ->
    ?ok(redis_client:request(Client, {"SUBSCRIBE", Channels})).

%%--------------------------------------------------------------------
%% @doc Returns the members of the set resulting from the union of
%% all the given sets.
%%
%% Redis command: [http://redis.io/commands/sunion SUNION]
%%
%% @spec sunion(Client, SetKeys) -> Union
%% Client = client()
%% SetKeys = [key()]
%% Union = [stored_value()]
%% @end
%%--------------------------------------------------------------------

sunion(Client, SetKeys) when length(SetKeys) > 1 ->
    ?term(redis_client:request(Client, {"SUNION", SetKeys})).

%%--------------------------------------------------------------------
%% @doc Same as `sunion' except that the result is stored at Destination.
%%
%% Redis command: [http://redis.io/commands/sunionstore SUNIONSTORE]
%%
%% @spec sdiffstore(Client, Destination, SetKeys) -> integer()
%% Client = client()
%% Destination = key()
%% SetKeys = [key()]
%% @end
%%--------------------------------------------------------------------

sunionstore(Client, Destination, SetKeys) when length(SetKeys) >  1 ->
    ?term(redis_client:request(
            Client, {"SUNIONSTORE", [Destination|SetKeys]})).

%%--------------------------------------------------------------------
%% @doc This is not implemented yet - not sure what this does.
%%
%% Redis command: [http://redis.io/commands/sync SYNC]
%%
%% @spec sync(Client) -> any()
%% @end
%%--------------------------------------------------------------------

sync(_Client) ->
    error(not_implemented).

%%--------------------------------------------------------------------
%% @doc Returns the remaining time to live of a key that has a timeout.
%%
%% Redis command: [http://redis.io/commands/ttl TTL]
%%
%% @spec ttl(Client, Key) -> integer()
%% @end
%%--------------------------------------------------------------------

ttl(Client, Key) ->
    ?term(redis_client:request(Client, {"TTL", [Key]})).

%%--------------------------------------------------------------------
%% @doc Returns the type of the value stored at Key.
%%
%% Redis command: [http://redis.io/commands/type TYPE]
%%
%% @spec type(Client, Key) -> Type
%% Client = client()
%% Key = key()
%% Type = string | list | set | zset | hash | none
%% @end
%%--------------------------------------------------------------------

type(Client, Key) ->
    list_to_atom(?term(redis_client:request(Client, {"TYPE", [Key]}))).

%%--------------------------------------------------------------------
%% @doc Unsubscribe from all channels.
%%
%% Redis command: [http://redis.io/commands/unsubscribe UNSUBSCRIBE]
%%
%% @spec unsubscribe(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

unsubscribe(Client) ->
    ?ok(redis_client:request(Client, {"UNSUBSCRIBE", []})).

%%--------------------------------------------------------------------
%% @doc Unsubscribes the client from the given channels.
%%
%% Redis command: [http://redis.io/commands/unsubscribe UNSUBSCRIBE]
%%
%% @spec unsubscribe(Client, Channels) -> ok
%% Client = client()
%% Chanels = [channel()]
%% @end
%%--------------------------------------------------------------------

unsubscribe(Client, Channels) ->
    ?ok(redis_client:request(Client, {"UNSUBSCRIBE", Channels})).

%%--------------------------------------------------------------------
%% @doc Flushes all the previously watched keys for a transaction.
%%
%% Redis command: [http://redis.io/commands/unwatch UNWATCH]
%%
%% @spec unwatch(Client) -> ok
%% Client = client()
%% @end
%%--------------------------------------------------------------------

unwatch(Client) ->
    ?ok(redis_client:request(Client, {"UNWATCH", []})).

%%--------------------------------------------------------------------
%% @doc Marks the given keys to be watched for conditional execution
%% of a transaction.
%%
%% Redis command: [http://redis.io/commands/watch WATCH]
%%
%% @spec watch(Client, Keys) -> ok
%% Client = client()
%% Keys = [key()]
%% @end
%%--------------------------------------------------------------------

watch(Client, Keys) when length(Keys) > 0 ->
    ?ok(redis_client:request(Client, {"WATCH", Keys})).

%%--------------------------------------------------------------------
%% @doc Adds all the specified members with the specified scores to
%% the sorted set stored at Key.
%%
%% Redis command: [http://redis.io/commands/zadd ZADD]
%%
%% @spec zadd(Client, Key, Members) -> integer()
%% Client = client()
%% Key = key()
%% Members = [scored_member()]
%% @end
%%--------------------------------------------------------------------

zmadd(Client, Key, Members) when length(Members) > 0 ->
    ?term(redis_client:request(
            Client, {"ZADD", [Key|scored_members_args(Members)]})).

%%--------------------------------------------------------------------
%% @doc Adds the specified member with the specified score to the
%% sorted set stored at Key.
%%
%% Redis command: [http://redis.io/commands/zadd ZADD]
%%
%% @spec zadd(Client, Key, {Score, Member}) -> integer()
%% @equiv zmadd(Client, Key, [{Score, Score}])
%% @end
%%--------------------------------------------------------------------

zadd(Client, Key, {Score, Member}) ->
    zmadd(Client, Key, [{Score, Member}]).

%%--------------------------------------------------------------------
%% @doc Adds the specified member with the specified score to the
%% sorted set stored at Key.
%%
%% Redis command: [http://redis.io/commands/zadd ZADD]
%%
%% @spec zadd(Client, Key, Score, Member) -> integer()
%% @equiv zmadd(Client, Key, [{Score, Member}])
%% @end
%%--------------------------------------------------------------------

zadd(Client, Key, Score, Member) ->
    zmadd(Client, Key, [{Score, Member}]).

%%--------------------------------------------------------------------
%% @doc Returns the number of elements of the sorted set stored at Key.
%%
%% Redis command: [http://redis.io/commands/zcard ZCARD]
%%
%% @spec zcard(Client, Key) -> integer()
%% Client = client()
%% Key = key()
%% @end
%%--------------------------------------------------------------------

zcard(Client, Key) ->
    ?term(redis_client:request(Client, {"ZCARD", [Key]})).

%%--------------------------------------------------------------------
%% @doc Returns the number of elements in the sorted set at Key with
%% a score between Min and Max.
%%
%% Redis command: [http://redis.io/commands/zcount ZCOUNT]
%%
%% @spec zcount(Client, Key, Min, Max) -> integer()
%% Client = client()
%% Key = key()
%% Min = number()
%% Max = number()
%% @end
%%--------------------------------------------------------------------

zcount(Client, Key, Min, Max) ->
    ?term(redis_client:request(
            Client, {"ZCOUNT", [Key, format_min_score(Min),
                                format_max_score(Max)]})).

%%--------------------------------------------------------------------
%% @doc Increments the score of member in the sorted set stored at
%% Key by Increment.
%%
%% Redis command: [http://redis.io/commands/zincrby ZINCRBY]
%%
%% @spec zincrby(Client, Key, Increment, Member) -> score()
%% Client = client()
%% Key = key()
%% Increment = number()
%% Member = value()
%% @end
%%--------------------------------------------------------------------

zincrby(Client, Key, Increment, Member) ->
    binary_to_number(
      ?term(redis_client:request(
              Client, {"ZINCRBY", [Key, format_number(Increment), Member]}))).

%%--------------------------------------------------------------------
%% @doc Computes the intersection of numkeys sorted sets given by the
%% specified keys, and stores the result in destination.
%%
%% Redis command: [http://redis.io/commands/zinterstore ZINTERSTORE]
%%
%% @spec zinterstore(Client, Destination, Keys) -> integer()
%% @equiv zinterstore(Client, Destination, Keys, [])
%% @end
%%--------------------------------------------------------------------

zinterstore(Client, Destination, Keys) when length(Keys) > 0 ->
    zinterstore(Client, Destination, Keys, []).

%%--------------------------------------------------------------------
%% @doc Computes the intersection of numkeys sorted sets given by the
%% specified keys, and stores the result in destination.
%%
%% Redis command: [http://redis.io/commands/zinterstore ZINTERSTORE]
%%
%% @spec zinterstore(Client, Destination, Keys, Options) -> integer()
%% Client = client()
%% Keys = [key() | {key(), number()}]
%% Options = [aggregate()]
%% @end
%%--------------------------------------------------------------------

zinterstore(Client, Destination, Keys, Options) when length(Keys) > 0 ->
    ?term(redis_client:request(
            Client, {"ZINTERSTORE", lists:append(
                                      [[Destination],
                                       weighted_keys_args(Keys),
                                       aggregate_args(Options)])})).

%%--------------------------------------------------------------------
%% @doc Returns the specified range of elements in the sorted set
%% stored at Key.
%%
%% Redis command: [http://redis.io/commands/zrange ZRANGE]
%%
%% @spec zrange(Client, Key, Start, Stop) -> Result
%% @equiv zrange(Client, Key, Start, Stop, [])
%% @end
%%--------------------------------------------------------------------

zrange(Client, Key, Start, Stop) ->
    zrange(Client, Key, Start, Stop, []).

%%--------------------------------------------------------------------
%% @doc Returns the specified range of elements in the sorted set
%% stored at Key.
%%
%% If withscores is provided as an option, the result is a list of tuples
%% consisting of the member and its score. If withscores is not provided,
%% the result is a list of the members.
%%
%% Redis command: [http://redis.io/commands/zrange ZRANGE]
%%
%% @spec zrange(Client, Key, Start, Stop, Options) -> Result
%% Client = client()
%% Key = key()
%% Start = integer()
%% Stop = integer()
%% Options = [withscores]
%% Result = [stored_value() | {stored_value(), score()}]
%% @end
%%--------------------------------------------------------------------

zrange(Client, Key, Start, Stop, Options) ->
    zrange_members(
      ?term(redis_client:request(
              Client, {"ZRANGE", lists:append(
                                   [Key, Start, Stop],
                                   withscores_args(Options))})),
      Options).

%%--------------------------------------------------------------------
%% @doc Returns all the elements in the sorted set at Key with a
%% score between Min and Max.
%%
%% Redis command: [http://redis.io/commands/zrangebyscore ZRANGEBYSCORE]
%%
%% @spec zrangebyscore(Client, Key, Min, Max) -> Result
%% @equiv zrangebyscore(Client, Key, Min, Max, [])
%% @end
%%--------------------------------------------------------------------

zrangebyscore(Client, Key, Min, Max) ->
    zrangebyscore(Client, Key, Min, Max, []).

%%--------------------------------------------------------------------
%% @doc Returns all the elements in the sorted set at Key with a
%% score between Min and Max.
%%
%% Redis command: [http://redis.io/commands/zrangebyscore ZRANGEBYSCORE]
%%
%% @spec zrangebyscore(Client, Key, Min, Max, Options) -> Result
%% Client = client()
%% Key = key()
%% Min = score()
%% Max = score()
%% Options = [withscores | {limit, Offset, Count}]
%% Offset = integer()
%% Count = integer()
%% @end
%%--------------------------------------------------------------------

zrangebyscore(Client, Key, Min, Max, Options) ->
    zrange_members(
      ?term(redis_client:request(
              Client, {"ZRANGEBYSCORE", lists:append(
                                          [[Key, format_min_score(Min),
                                            format_max_score(Max)],
                                           withscores_args(Options),
                                           limit_args(Options)])})),
      Options).

%%--------------------------------------------------------------------
%% @doc Returns the rank of member in the sorted set stored at Key.
%%
%% Redis command: [http://redis.io/commands/zrank ZRANK]
%%
%% @spec zrank(Client, Key, Member) -> {ok, integer()} | undefined
%% Client = client()
%% Key = key()
%% Member = member()
%% @end
%%--------------------------------------------------------------------

zrank(Client, Key, Member) ->
    ?maybe_term(redis_client:request(Client, {"ZRANK", [Key, Member]})).

%%--------------------------------------------------------------------
%% @doc Removes the specified members from the sorted set stored at Key.
%%
%% Redis command: [http://redis.io/commands/zrem ZREM]
%%
%% @spec zmrem(Client, Key, Members) -> integer()
%% Client = client()
%% Key = key()
%% Members = [member()]
%% @end
%%--------------------------------------------------------------------

zmrem(Client, Key, Members) when length(Members) > 0 ->
    ?term(redis_client:request(Client, {"ZREM", [Key|Members]})).


%%--------------------------------------------------------------------
%% @doc Removes a single member from an ordered set.
%%
%% Redis command: [http://redis.io/commands/zrem ZREM]
%%
%% @spec zrem(Client, Key, Member) -> integer()
%% @equiv zmrem(Client, Key, [Member])
%% @end
%%--------------------------------------------------------------------

zrem(Client, Key, Member) ->
    ?term(redis_client:request(Client, {"ZREM", [Key, Member]})).

%%--------------------------------------------------------------------
%% @doc Removes all elements in the sorted set stored at Key with rank
%% between Start and Stop.
%%
%% Redis command: [http://redis.io/commands/zremrangebyrank ZREMRANGEBYRANK]
%%
%% @spec zremrangebyrank(Client, Key, Start, Stop) -> integer()
%% Client = client()
%% Key = key()
%% Start = integer()
%% Stop = integer()
%% @end
%%--------------------------------------------------------------------

zremrangebyrank(Client, Key, Start, Stop) ->
    ?term(redis_client:request(
            Client, {"ZREMRANGEBYRANK", [Key, Start, Stop]})).

%%--------------------------------------------------------------------
%% @doc Removes all elements in the sorted set stored at Key with a
%% score between Min and Max (inclusive).
%%
%% Redis command: [http://redis.io/commands/zremrangebyscore ZREMRANGEBYSCORE]
%%
%% @spec zremrangebyscore(Client, Key, Min, Max) -> integer()
%% Client = client()
%% Key = key()
%% Min = number()
%% Max = number()
%% @end
%%--------------------------------------------------------------------

zremrangebyscore(Client, Key, Min, Max) ->
    ?term(redis_client:request(
            Client, {"ZREMRANGEBYSCORE", [Key, format_min_score(Min),
                                          format_max_score(Max)]})).

%%--------------------------------------------------------------------
%% @doc Returns the specified range of elements in the sorted set
%% stored at Key.
%%
%% Redis command: [http://redis.io/commands/zrevrange ZREVRANGE]
%%
%% @spec zrevrange(Client, Key, Start, Stop, Options) -> Result
%% @equiv zrevrange(Client, Key, Start, Stop, []).
%% @end
%%--------------------------------------------------------------------

zrevrange(Client, Key, Start, Stop) ->
    zrevrange(Client, Key, Start, Stop, []).

%%--------------------------------------------------------------------
%% @doc Returns the specified range of elements in the sorted set
%% stored at Key.
%%
%% Redis command: [http://redis.io/commands/zrevrange ZREVRANGE]
%%
%% @spec zrevrange(Client, Key, Start, Stop, Options) -> Result
%% Client = client()
%% Key = key()
%% Start = integer()
%% Stop = integer()
%% Options = [withscores]
%% Result = [stored_value() | {stored_score(), value()}]
%% @end
%%--------------------------------------------------------------------

zrevrange(Client, Key, Start, Stop, Options) ->
    zrange_members(
      ?term(redis_client:request(
              Client, {"ZREVRANGE", lists:append(
                                      [Key, Start, Stop],
                                      withscores_args(Options))})),
      Options).

%%--------------------------------------------------------------------
%% @doc Returns all the elements in the sorted set at Key with a
%% score between Max and Min.
%%
%% Redis command: [http://redis.io/commands/zrevrangebyscore ZREVRANGEBYSCORE]
%%
%% @spec zrevrangebyscore(Client, Key, Max, Min, Options) -> Result
%% @equiv zrevrangebyscore(Client, Key, Max, Min, [])
%% @end
%%--------------------------------------------------------------------

zrevrangebyscore(Client, Key, Max, Min) ->
    zrevrangebyscore(Client, Key, Max, Min, []).

%%--------------------------------------------------------------------
%% @doc Returns all the elements in the sorted set at Key with a
%% score between Max and Min.
%%
%% Redis command: [http://redis.io/commands/zrevrangebyscore ZREVRANGEBYSCORE]
%%
%% @spec zrevrangebyscore(Client, Key, Max, Min, Options) -> Result
%% Client = client()
%% Key = key()
%% Max = number()
%% Min = number()
%% Options = [withscores | {limit, Offset, Count}]
%% Offset = integer()
%% Count = integer()
%% Result = [stored_value() | {stored_score(), value()}]
%% @end
%%--------------------------------------------------------------------

zrevrangebyscore(Client, Key, Max, Min, Options) ->
    zrange_members(
      ?term(redis_client:request(
              Client, {"ZREVRANGEBYSCORE", lists:append(
                                             [[Key, format_max_score(Max),
                                               format_min_score(Min)],
                                              withscores_args(Options),
                                              limit_args(Options)])})),
      Options).

%%--------------------------------------------------------------------
%% @doc Returns the rank of member in the sorted set stored at Key,
%% with the scores ordered from high to low.
%%
%% Redis command: [http://redis.io/commands/zrevrank ZREVRANK]
%%
%% @spec zrevrank(Client, Key, Member) -> {ok, integer()} | undefined
%% Client = client()
%% Key = key()
%% Member = member()
%% @end
%%--------------------------------------------------------------------

zrevrank(Client, Key, Member) ->
    ?maybe_term(redis_client:request(Client, {"ZREVRANK", [Key, Member]})).

%%--------------------------------------------------------------------
%% @doc Returns the score of member in the sorted set at key.
%%
%% Redis command: [http://redis.io/commands/zscore ZSCORE]
%%
%% @spec zscore(Client, Key, Member) -> {ok, score()} | undefined
%% Client = client()
%% Key = key()
%% Member = member()
%% @end
%%--------------------------------------------------------------------

zscore(Client, Key, Member) ->
    case ?maybe_term(redis_client:request(
                       Client, {"ZSCORE", [Key, Member]})) of
        {ok, Score} -> {ok, binary_to_number(Score)};
        undefined -> undefined
    end.

%%--------------------------------------------------------------------
%% @doc Computes the union of numkeys sorted sets given by the
%% specified keys, and stores the result in Destination.
%%
%% Redis command: [http://redis.io/commands/zunionstore ZUNIONSTORE]
%%
%% @spec zunionstore(Client, Destination, Keys) -> integer()
%% @equiv zunionstore(Client, Destination, Keys, [])
%% @end
%%--------------------------------------------------------------------

zunionstore(Client, Destination, Keys) ->
    zunionstore(Client, Destination, Keys, []).

%%--------------------------------------------------------------------
%% @doc Computes the union of numkeys sorted sets given by the
%% specified keys, and stores the result in Destination.
%%
%% Redis command: [http://redis.io/commands/zunionstore ZUNIONSTORE]
%%
%% @spec zunionstore(Client, Destination, Keys, Options) -> integer()
%% Client = client()
%% Keys = [key() | {key(), number()}]
%% Options = [aggregate()]
%% @end
%%--------------------------------------------------------------------

zunionstore(Client, Destination, Keys, Options) when length(Keys) > 0 ->
    ?term(redis_client:request(
            Client, {"ZUNIONSTORE", lists:append(
                                      [[Destination],
                                       weighted_keys_args(Keys),
                                       aggregate_args(Options)])})).

%%%===================================================================
%%% Internal functions
%%%===================================================================

request_timeout(Timeout) -> Timeout * 1000 + 1000.

where_arg(before) -> <<"BEFORE">>;
where_arg('after') -> <<"AFTER">>;
where_arg(Other) -> error({badarg, Other}).

bulk_proplist(Bin) ->
    [bulk_prop(P) || P <- binary:split(Bin, <<"\r\n">>, [global, trim])].

bulk_prop(Bin) ->
    [Name, Val] = binary:split(Bin, <<":">>),
    {binary_to_propname(Name), binary_to_propval(Val)}.

multi_proplist(Values) ->
    multi_proplist(Values, []).

multi_proplist([], Acc) -> lists:reverse(Acc);
multi_proplist([Key, Value|Rest], Acc) ->
    Prop = {binary_to_list(Key), maybe_binary_to_propval(Value)},
    multi_proplist(Rest, [Prop|Acc]).

binary_to_propname(Bin) ->
    list_to_atom(binary_to_list(Bin)).

maybe_binary_to_propval(undefined) ->
    undefined;
maybe_binary_to_propval(Bin) when is_binary(Bin) ->
    binary_to_propval(Bin).

binary_to_propval(Bin) ->
    S = binary_to_list(Bin),
    try list_to_float(S) of
        F -> F
    catch
        error:badarg ->
            try list_to_integer(S) of
                I -> I
            catch
                error:badarg -> S
            end
    end.

multi_hashlist(Values) ->
    multi_hashlist(Values, []).

multi_hashlist([], Acc) -> lists:reverse(Acc);
multi_hashlist([Name, Val|Rest], Acc) ->
    multi_hashlist(Rest, [{Name, Val}|Acc]).

field_value_args(FieldValues) ->
    lists:concat([[Field, Value] || {Field, Value} <- FieldValues]).

key_value_args(KeyValues) ->
    lists:concat([[Key, Value] || {Key, Value} <- KeyValues]).

object_subcommand(A) when is_atom(A) ->
    atom_to_list(A).

sort_args(Options) ->
    sort_args(proplists:compact(Options), []).

sort_args([], Acc) -> lists:concat(Acc);
sort_args([{limit, Offset, Count}|Rest], Acc) ->
    sort_args(Rest, [["LIMIT", Offset, Count]|Acc]);
sort_args([asc|Rest], Acc) ->
    sort_args(Rest, [["ASC"]|Acc]);
sort_args([desc|Rest], Acc) ->
    sort_args(Rest, [["DESC"]|Acc]);
sort_args([alpha|Rest], Acc) ->
    sort_args(Rest, [["ALPHA"]|Acc]);
sort_args([{by, By}|Rest], Acc) ->
    sort_args(Rest, [["BY", By]|Acc]);
sort_args([{get, Get}|Rest], Acc) ->
    sort_args(Rest, [["GET", Get]|Acc]);
sort_args([{mget, Gets}|Rest], Acc) ->
    GetCmds = lists:concat([["GET", G] || G <- Gets]),
    sort_args(Rest, [GetCmds|Acc]);
sort_args([{store, Dest}|Rest], Acc) ->
    sort_args(Rest, [["STORE", Dest]|Acc]);
sort_args([nosort|Rest], Acc) ->
    sort_args(Rest, [["BY", "NOSORT"]|Acc]);
sort_args([Other|_], _) ->
    error({badarg, Other}).

scored_members_args(Members) ->
    scored_members_args(Members, []).

scored_members_args([], Acc) -> lists:concat(lists:reverse(Acc));
scored_members_args([{Score, Member}|Rest], Acc) when is_number(Score) ->
    scored_members_args(Rest, [[format_number(Score), Member]|Acc]);
scored_members_args([Other|_], _) ->
    error({badarg, Other}).

sort_result(RawResults, Options) ->
    case proplists:get_value(mget, Options) of
        undefined -> RawResults;
        Gets -> group_sort_results(RawResults, length(Gets))
    end.

group_sort_results(Results, GroupLen) ->
    group_sort_results(Results, GroupLen, []).

group_sort_results([], _, Acc) -> lists:reverse(Acc);
group_sort_results(Results, GroupLen, Acc) ->
    {Group, Rest} = lists:split(GroupLen, Results),
    group_sort_results(Rest, GroupLen, [Group|Acc]).

format_number(I) when is_integer(I) -> list_to_binary(integer_to_list(I));
format_number(F) when is_float(F) -> io_lib:format("~.8f", [F]);
format_number(Other) -> Other.

format_min_score(infinity) -> "-inf";
format_min_score(Min) -> format_number(Min).

format_max_score(infinity) -> "+inf";
format_max_score(Max) -> format_number(Max).

weighted_keys_args(Keys) ->
    lists:append(
      [[length(Keys)],
       lists:map(fun weighted_key_key/1, Keys),
       ["WEIGHTS"],
       lists:map(fun weighted_key_weight/1, Keys)]).

weighted_key_key({Key, _Weight}) -> Key;
weighted_key_key(Key) -> Key.

weighted_key_weight({_Key, Weight}) when is_number(Weight) ->
    format_number(Weight);
weighted_key_weight({_Key, Weight}) -> Weight;
weighted_key_weight(_Key) -> "1".

aggregate_args([]) -> [];
aggregate_args([sum|_]) -> ["AGGREGATE", "SUM"];
aggregate_args([min|_]) -> ["AGGREGATE", "MIN"];
aggregate_args([max|_]) -> ["AGGREGATE", "MAX"];
aggregate_args([_|Rest]) -> aggregate_args(Rest).

withscores_args([]) -> [];
withscores_args([withscores|_]) -> ["WITHSCORES"];
withscores_args([_|Rest]) -> withscores_args(Rest).

limit_args([]) -> [];
limit_args([{limit, Offset, Count}|_]) -> ["LIMIT", Offset, Count];
limit_args([_|Rest]) -> limit_args(Rest).

zrange_members(ZRangeResult, Options) ->
    case proplists:get_bool(withscores, Options) of
        true -> members_with_score(ZRangeResult, []);
        false -> ZRangeResult
    end.

members_with_score([], Acc) -> lists:reverse(Acc);
members_with_score([Member, Score|Rest], Acc) ->
    members_with_score(Rest, [{Member, binary_to_number(Score)}|Acc]).

binary_to_number(B) ->
    S = binary_to_list(B),
    try list_to_integer(S) of
        I -> I
    catch
        error:badarg ->
            list_to_float(S)
    end.
