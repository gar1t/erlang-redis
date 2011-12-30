-module(lf_id).

-behavior(e2_service).

-export([start_link/0,
         user_id/0]).

-export([handle_user_id/2]).

-record(state, {}).

start_link() ->
    e2_service:start_link(?MODULE, #state{}, [registered]).

user_id() ->
    e2_service:call(?MODULE, {handle_user_id, []}).

handle_user_id(_From, State) ->
    {reply, "hello!", State}.
