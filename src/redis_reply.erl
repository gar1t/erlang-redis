-module(redis_reply).

-export([new/0, data/2]).

-define(CRLF, "\r\n").

-record(reply, {value}).

new() -> #reply{value={unknown, <<>>}}.

new(Data) -> #reply{value={unknown, Data}}.

data(In, #reply{value={bulk, Acc}}) ->
    handle_bulk(<<Acc/binary, In/binary>>);
data(In, #reply{value={bulk, Size, Acc}}) ->
    handle_bulk(Size, <<Acc/binary, In/binary>>);
data(In, #reply{value={message, Acc}}) ->
    handle_message(<<Acc/binary, In/binary>>);
data(In, #reply{value={error, Acc}}) ->
    handle_error(<<Acc/binary, In/binary>>);
data(In, #reply{value={unknown, Acc}}) ->
    handle_unknown(<<Acc/binary, In/binary>>).

handle_bulk(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            handle_bulk(parse_integer(Line), Rest);
        false ->
            pending({bulk, Data})
    end.

handle_bulk(-1, Data) ->
    reply(undefined, new(Data));
handle_bulk(Size, Data) ->
    case split_bulk(Data, Size) of
        {Value, Rest} ->
            reply({ok, Value}, new(Rest));
        false ->
            pending({bulk, Size, Data})
    end.

handle_message(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            reply({ok, parse_message(Line)}, new(Rest));
        false ->
            pending({message, Data})
    end.

handle_integer(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            reply({ok, parse_integer(Line)}, new(Rest));
        false ->
            pending(#reply{value={integer, Data}})
    end.

handle_error(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            reply({error, parse_message(Line)}, new(Rest));
        false ->
            pending({error, Data})
    end.

handle_unknown(<<"\$", Data/binary>>) ->
    handle_bulk(Data);
handle_unknown(<<":", Data/binary>>) ->
    handle_integer(Data);
handle_unknown(<<"+", Data/binary>>) ->
    handle_message(Data);
handle_unknown(<<"-", Data/binary>>) ->
    handle_error(Data);
handle_unknown(Other) ->
    pending(#reply{value={unknown, Other}}).

reply(Value, Reply) ->
    {{value, Value}, Reply}.

pending(Reply) ->
    {pending, Reply}.

parse_integer(Data) ->
    list_to_integer(binary_to_list(Data)).

parse_message(Data) ->
    binary_to_list(Data).

parse_error(Data) ->
    binary_to_list(Data).

split_crlf(Data) ->
    split_crlf(Data, Data, 0).

split_crlf(_Data, <<>>, _Count) -> false;
split_crlf(Data, <<?CRLF, Rest/binary>>, Count) ->
    {binary:part(Data, 0, Count), Rest};
split_crlf(Data, <<_:8, Rest/binary>>, Count) ->
    split_crlf(Data, Rest, Count + 1).

split_bulk(Data, Size) ->
    if
        byte_size(Data) >= Size + length(?CRLF) ->
            <<Value:Size/binary, ?CRLF, Rest/binary>> = Data,
            {Value, Rest};
        true ->
            false
    end.

-compile(export_all).
