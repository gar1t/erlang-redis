-module(redis_client).

-behaviour(gen_server).

-export([start_link/2, request/2, request/3, quit/1, quit/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {socket, requests, reply, stopping}).

-define(DEFAULT_REQUEST_TIMEOUT, 5000).
-define(DEFAULT_STOP_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Host, Port], []).

request(Client, Request) ->
    request(Client, validate_request(Request), ?DEFAULT_REQUEST_TIMEOUT).

request(Client, {_Cmd, _Args}=Request, Timeout) ->
    gen_server:call(Client, {request, Request}, Timeout).

quit(Client) ->
    quit(Client, ?DEFAULT_STOP_TIMEOUT).

quit(Client, Timeout) ->
    gen_server:call(Client, quit, Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    case connect(Host, Port) of
        {ok, Socket} ->
            {ok, init_state(Socket)};
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
            {noreply, reply(Value, State#state{reply=Next})};
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

init_state(Socket) ->
    #state{socket=Socket,
           requests=queue:new(),
           reply=redis_reply:new()}.

connect(Host, Port) ->
    gen_tcp:connect(Host, Port, [binary, {active, once}, {packet, raw}, 
                                 {reuseaddr, true}]).

send_request({Cmd, Args}, From, #state{requests=Reqs, socket=Socket}=State) ->
    ok = redis_cmd:send(Socket, Cmd, Args),
    State#state{requests=queue:in(From, Reqs)}.

reply(Value, #state{requests=Requests0}=State) ->
    {{value, From}, Requests} = queue:out(Requests0),
    gen_server:reply(From, erlang_value(Value)),
    State#state{requests=Requests}.

erlang_value({ok, Int}) when is_integer(Int) -> {ok, Int}; 
erlang_value({ok, Bin}) when is_binary(Bin) -> {ok, Bin};
erlang_value({ok, "OK"}) -> ok;
erlang_value({ok, List}) when is_list(List) -> {ok, List};
erlang_value({error, "ERR " ++ Msg}) -> {error, Msg};
erlang_value(undefined) -> undefined.
