-module(redis_test_server).

-behaviour(gen_server).

-export([start_link/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {pidfile}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop(Port) ->
    gen_server:call(Port, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    process_flag(trap_exit, true),
    {ok, #state{pidfile=start_redis(Port)}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{pidfile=PidFile}) ->
    ok = stop_redis(PidFile).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_redis(SocketPort) ->
    PidFile = pidfile_name(),
    delete_pidfile(PidFile),
    Port = erlang:open_port({spawn, "redis-server -"}, []),
    erlang:port_command(Port, ["port ", integer_to_list(SocketPort), "\n"]),
    erlang:port_command(Port, ["daemonize yes\n"]),
    erlang:port_command(Port, ["pidfile ", PidFile, "\n"]),
    erlang:port_close(Port),
    waitfor_pid(PidFile, 3).

pidfile_name() ->
    "/tmp/erlang-redis-test-server.pid".

delete_pidfile(File) ->
    case file:delete(File) of
        ok -> ok;
        {error, enoent} -> ok
    end.

waitfor_pid(File, 0) -> exit({no_pidfile, File});
waitfor_pid(File, N) ->
    case file:read_file(File) of
        {ok, Bin} -> parse_pid(Bin);
        {error, enoent} ->
            timer:sleep(50),
            waitfor_pid(File, N - 1)
    end.

parse_pid(Bin) ->
    case re:run(Bin, "(\\d+)", [{capture, [1], list}]) of
        {match, [Pid]} -> list_to_integer(Pid);
        nomatch -> exit({bad_pidfile_content, Bin})
    end.

stop_redis(Pid) ->
    case os:cmd("kill " ++ integer_to_list(Pid)) of
        [] -> ok;
        Err -> {error, Err}
    end.
