-module(jn_component_sup).

-behaviour(supervisor).

-include("../include/jn_component.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(State) when is_record(State, jnstate) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, State).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(#jnstate{maxPerPeriod = MaxPerPeriod, periodSeconds = PeriodSeconds,initPort = InitPort, endPort = EndPort}=State) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Children = [
        {jn_component, {jn_component, start_link, [State]}, Restart, Shutdown, Type, [jn_component]},
        {jn_schedule, {jn_schedule, start_link, [MaxPerPeriod,PeriodSeconds]}, Restart, Shutdown, Type, [jn_schedule]},
        {jn_portmonitor, {jn_portmonitor, start_link, [InitPort,EndPort]}, Restart, Shutdown, Type, [jn_portmonitor]}
    ],
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
