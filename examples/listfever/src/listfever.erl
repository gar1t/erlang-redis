-module(listfever).

-behavior(e2_application).

-export([start/0, init/0, priv_dir/0]).

start() ->
    e2_application:start_with_dependencies(listfever).

init() ->
    {ok, [lf_id, lf_user]}.

priv_dir() ->
    case code:priv_dir(listfever) of
        {error, bad_name} ->
            %% We haven't been started as an app - hack by looking for beam
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join([filename:dirname(Ebin), "priv"]);
        Dir -> Dir
    end.
