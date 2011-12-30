-module(lf_user).

-behavior(e2_service).

-export([start_link/0,
         get/1]).

-export([init/1, handle_redis_call/4]).

-record(state, {redis}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

%%--------------------------------------------------------------------
%% @doc Returns the user associated with Key.
%% @spec get(Key) -> {ok, User} | undefined
%% @end
%%--------------------------------------------------------------------

get(Key) ->
    e2_service:call(?MODULE, {handle_redis_call, [hgetall, [user_key(Key)]]}).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init([]) ->
    {ok, R} = connect_redis(),
    {ok, #state{redis=R}}.

handle_redis_call(F, A, _From, #state{redis=R}=State) ->
    {reply, apply(redis, F, [R|A]), State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect_redis() ->
    %% TODO: Use config for redis connection settings
    redis:connect().

user_key(Key) ->
    ["u:", Key].
