-module(redis_cmd).

-export([call/3, send/3, recv/1]).

-define(CRLF, "\r\n").

call(Socket, Cmd, Args) ->
    case send(Socket, Cmd, Args) of
        ok -> recv(Socket);
        {error, Err} -> {error, Err}
    end.

send(Socket, Cmd, Args) ->
    SendArgs = [Cmd|Args],
    case gen_tcp:send(Socket, ["*", arg_count(SendArgs), ?CRLF]) of
        ok ->
            send_args(Socket, SendArgs);
        {error, Err} ->
            {error, Err}
    end.

send_args(_Socket, []) -> ok;
send_args(Socket, [Arg|Rest]) -> 
    case gen_tcp:send(Socket, ["\$", arg_size(Arg), ?CRLF, Arg, ?CRLF]) of
        ok -> send_args(Socket, Rest);
        {error, Err} -> {error, Err}
    end.

arg_count(Args) ->
    integer_to_list(length(Args)).

arg_size(Arg) ->
    integer_to_list(iolist_size(Arg)).

recv(Socket) ->
    recv(Socket, redis_reply:new()).

recv(Socket, Reply) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} -> redis_reply:data(Data, Reply);
        {error, Err} -> {error, Err}
    end.    

