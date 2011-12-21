-module(redis_client).

-include("redis.hrl").

-behaviour(gen_server).

-export([start_link/1, request/2, request/3, quit/1, quit/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {socket, requests, reply, stopping, recipient}).

-define(DEFAULT_STOP_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Options) ->
    gen_server:start_link(?MODULE, [self(), Options], []).

request(Client, Request) ->
    request(Client, Request, infinity).

request(Client, {_Cmd, _Args}=Request, Timeout) ->
    gen_server:call(Client, {request, validate_request(Request)}, Timeout).

quit(Client) ->
    quit(Client, ?DEFAULT_STOP_TIMEOUT).

quit(Client, Timeout) ->
    gen_server:call(Client, quit, Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Parent, Options]) ->
    case connect(Options) of
        {ok, Socket} ->
            {ok, init_state(Socket, Parent, Options)};
        {error, Err} ->
            {stop, {connect, Err}}
    end.

handle_call(_, _From, #state{stopping=true}=State) ->
    {reply, {error, stopping}, State};
handle_call({request, {_Cmd, _Args}=Request}, From, State) ->
    {noreply, send_request(Request, From, State)};
handle_call(quit, From, State) ->
    {noreply, send_request({"QUIT", []}, From, State#state{stopping=true})}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, #state{reply=Reply}=State) ->
    inet:setopts(Socket, [{active, once}]),
    case redis_reply:data(Data, Reply) of
        {{value, Value}, Next} ->
            {noreply, handle_value(Value, State#state{reply=Next})};
        {pending, Next} ->
            {noreply, State#state{reply=Next}}
    end;
handle_info({tcp_closed, _}, #state{stopping=true}=State) ->
    {stop, normal, State};
handle_info({tcp_closed, Socket}, State) ->
    {stop, {connection_closed, Socket}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_request({Cmd, Args}) ->
    {validate_cmd(Cmd), lists:map(fun validate_arg/1, Args)}.

validate_cmd(Cmd) when is_list(Cmd) -> Cmd;
validate_cmd(Cmd) when is_binary(Cmd) -> Cmd;
validate_cmd(Cmd) -> error({badcmd, Cmd}).

validate_arg(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I));
validate_arg(S) ->
    try iolist_to_binary(S) of
        Bin -> Bin
    catch
        error:badarg -> error({badarg, S})
    end.

init_state(Socket, Parent, Options) ->
    #state{socket=Socket,
           requests=queue:new(),
           reply=redis_reply:new(),
           recipient=proplists:get_value(recipient, Options, Parent)}.

connect(Options) ->
    Host = proplists:get_value(host, Options, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    gen_tcp:connect(Host, Port, [binary, {active, once}, {packet, raw}, 
                                 {reuseaddr, true}]).

send_request({Cmd, Args}, From, #state{requests=Reqs, socket=Socket}=State) ->
    ok = redis_cmd:send(Socket, Cmd, Args),
    State#state{requests=queue:in(From, Reqs)}.

handle_value(Value, #state{requests=Requests0}=State) ->
    case queue:out(Requests0) of
        {{value, From}, Requests} ->
            gen_server:reply(From, erlang_value(Value)),
            State#state{requests=Requests};
        {empty, _} ->
            publish_value(Value, State),
            State
    end.

publish_value({ok, [<<"message">>, Channel, Msg]},
              #state{recipient=Dest}) ->
    dispatch_message({message, Channel, Msg}, Dest);
publish_value({ok, [<<"pmessage">>, Pattern, Channel, Msg]},
              #state{recipient=Dest}) ->
    dispatch_message({pmessage, Pattern, Channel, Msg}, Dest).

dispatch_message(Msg, Fun) when is_function(Fun) ->
    Fun(Msg);
dispatch_message(Msg, {M, F, A}) ->
    erlang:apply(M, F, A ++ [Msg]); 
dispatch_message(Msg, Proc) when is_pid(Proc); is_atom(Proc) ->
    erlang:send(Proc, Msg).

erlang_value({ok, Int}) when is_integer(Int) -> {ok, Int}; 
erlang_value({ok, Bin}) when is_binary(Bin) -> {ok, Bin};
erlang_value({ok, "OK"}) -> ok;
erlang_value({ok, List}) when is_list(List) -> {ok, List};
erlang_value({error, "ERR " ++ Msg}) -> {error, Msg};
erlang_value(undefined) -> undefined.
