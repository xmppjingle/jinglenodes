-module(jn_component_app).

-behaviour(application).

-include("../include/jn_component.hrl").

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start() ->
	application:start(jn_component).

start(_StartType, _StartArgs) ->
    ?INFO_MSG("Loading Application",[]),
    [Conf] = confetti:fetch(mgmt_conf),
    prepare_tables(),
    JNConf = proplists:get_value(jn_component, Conf, []),
    {InitPort, EndPort} = proplists:get_value(port_range, JNConf),
    {MaxPerPeriod, PeriodSeconds} = proplists:get_value(throttle, JNConf),
    jn_component_sup:start_link(#jnstate{
        pubIP = proplists:get_value(public_ip, JNConf),
        jid = proplists:get_value(jid, JNConf),
        whiteDomain = proplists:get_value(whitelist, JNConf),
        maxPerPeriod = MaxPerPeriod,
        initPort = InitPort,
        endPort = EndPort,
        periodSeconds = PeriodSeconds,
        handler = proplists:get_value(handler, JNConf),
        broadcast = proplists:get_value(broadcast, JNConf),
        discount = proplists:get_value(discount, JNConf, 0)
    }).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(State) ->
    ?INFO_MSG("Terminating: ~p~n",[State]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

prepare_tables() ->
    mnesia:create_table(jn_relay_service,
            [{disc_only_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, jn_relay_service)}]),
    mnesia:create_table(jn_tracker_service,
            [{disc_only_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, jn_tracker_service)}]).
