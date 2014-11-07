-module(redis_reply).

-export([new/0, data/2]).

-define(CRLF, "\r\n").

new() -> {unknown, <<>>}.

data(In, {bulk, Acc}) ->
    handle_bulk(<<Acc/binary, In/binary>>);
data(In, {bulk, Size, Acc}) ->
    handle_bulk(Size, <<Acc/binary, In/binary>>);
data(In, {multi, Acc}) ->
    handle_multi(<<Acc/binary, In/binary>>);
data(In, {multi, Count, Values, CurValue}) ->
    handle_multi(Count, Values, CurValue, In);
data(In, {message, Acc}) ->
    handle_message(<<Acc/binary, In/binary>>);
data(In, {error, Acc}) ->
    handle_error(<<Acc/binary, In/binary>>);
data(In, {integer, Acc}) ->
    handle_integer(<<Acc/binary, In/binary>>);
data(In, {unknown, Acc}) ->
    handle_unknown(<<Acc/binary, In/binary>>).

handle_bulk(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            handle_bulk(parse_integer(Line), Rest);
        false ->
            {pending, {bulk, Data}}
    end.

handle_bulk(-1, Data) ->
    {{value, undefined}, {unknown, Data}};
handle_bulk(Size, Data) ->
    case have_bulk_value(Size, Data) of
        {true, {Value, Rest}} ->
            {{value, {ok, Value}}, {unknown, Rest}};
        false ->
            {pending, {bulk, Size, Data}}
    end.

handle_multi(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            handle_multi(parse_integer(Line), [], {unknown, <<>>}, Rest);
        false ->
            {pending, {multi, Data}}
    end.

handle_multi(-1, [], {unknown, <<>>}, Data) ->
    {{value, undefined}, {unknown, Data}};
handle_multi(Count, Values, CurValue, <<>>)
  when length(Values) == Count ->
    {{value, {ok, lists:reverse(Values)}}, CurValue};
handle_multi(Count, Values, CurValue, Data) ->
    case data(Data, CurValue) of
        {{value, Value}, NextValue} ->
            handle_multi(Count, [multi_value(Value)|Values], NextValue, <<>>);
        {pending, NextValue} ->
            {pending, {multi, Count, Values, NextValue}}
    end.

multi_value(undefined) -> undefined;
multi_value({ok, Value}) -> Value.

handle_message(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            {{value, {ok, parse_message(Line)}}, {unknown, Rest}};
        false ->
            {pending, {message, Data}}
    end.

handle_integer(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            {{value, {ok, parse_integer(Line)}}, {unknown, Rest}};
        false ->
            {pending, {integer, Data}}
    end.

handle_error(Data) ->
    case split_crlf(Data) of
        {Line, Rest} ->
            {{value, {error, parse_message(Line)}}, {unknown, Rest}};
        false ->
            {pending, {error, Data}}
    end.

handle_unknown(<<"\$", Data/binary>>) ->
    handle_bulk(Data);
handle_unknown(<<"*", Data/binary>>) ->
    handle_multi(Data);
handle_unknown(<<":", Data/binary>>) ->
    handle_integer(Data);
handle_unknown(<<"+", Data/binary>>) ->
    handle_message(Data);
handle_unknown(<<"-", Data/binary>>) ->
    handle_error(Data);
handle_unknown(<<>>) ->
    {pending, {unknown, <<>>}}.

parse_integer(Data) ->
    list_to_integer(binary_to_list(Data)).

parse_message(Data) ->
    binary_to_list(Data).

split_crlf(Data) ->
    split_crlf(Data, Data, 0).

split_crlf(_Data, <<>>, _Count) -> false;
split_crlf(Data, <<?CRLF, Rest/binary>>, Count) ->
    {binary:part(Data, 0, Count), Rest};
split_crlf(Data, <<_:8, Rest/binary>>, Count) ->
    split_crlf(Data, Rest, Count + 1).

have_bulk_value(Size, Data) ->
    if
        byte_size(Data) >= Size + length(?CRLF) ->
            <<Value:Size/binary, ?CRLF, Rest/binary>> = Data,
            {true, {Value, Rest}};
        true ->
            false
    end.
