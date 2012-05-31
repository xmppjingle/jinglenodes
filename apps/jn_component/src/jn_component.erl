%%%-------------------------------------------------------------------
%%% File    : jn_component.erl
%%% Author  : Thiago Camargo <barata7@gmail.com>
%%% Description : Jingle Nodes Services - External Component
%%% Provides:
%%%		* UDP Relay Services
%%%
%%% Created : 01 Nov 2009 by Thiago Camargo <barata7@gmail.com>
%%%-------------------------------------------------------------------

-module(jn_component).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-import(config).
-import(file).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/jn_component.hrl").

%% API
-export([get_stats/0, prepare_id/1, unprepare_id/1, is_allowed/2, get_port/1]).

%% gen_server callbacks
-export([start_link/0, init/12, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) -> init(["./etc/jn_component.cfg"]);
init([CFile]) ->
	?INFO_MSG("Loading Application",[]),
	case file:consult(CFile) of
		{ok, Cfg} ->
			?INFO_MSG("Loading Config...~n",[]),
			init(                   get(jid, Cfg),
                                get(pass, Cfg),
                                get(server, Cfg),
                                get(port, Cfg),
                                get(public_ip, Cfg),
                                get(channel_timeout, Cfg),
                                get(whitelist, Cfg),
                                get(max_per_period, Cfg),
                                get(period_seconds, Cfg),
                                get(init_port, Cfg),
                                get(end_port, Cfg),
			        get(handler, Cfg));
		_ -> 	
				?INFO_MSG("Invalid Config File[~p] ~n",[filename:absname("")]),
				{error, config_missing}
	end.

init(JID, Pass, Server, Port, PubIP, ChannelTimeout, WhiteDomain, MaxPerPeriod, PeriodSeconds, InitPort, EndPort, Handler) ->
    mnesia:create_table(jn_relay_service,
            [{disc_only_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, jn_relay_service)}]),
    mnesia:create_table(jn_tracker_service,
            [{disc_only_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, jn_tracker_service)}]),
    application:start(exmpp),
    mod_monitor:init(),
    ChannelMonitor = scheduleChannelPurge(5000, [], ChannelTimeout),
    PortMonitor = schedulePortMonitor(InitPort, EndPort),
    {_, XmppCom} = make_connection(JID, Pass, Server, Port),
    {ok, #state{xmppCom=XmppCom, jid=JID, pass=Pass, server=Server, port=Port, pubIP=PubIP, channelMonitor=ChannelMonitor, whiteDomain=[list_to_binary(S) || S <- string:tokens(WhiteDomain, ",")], maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds, portMonitor=PortMonitor, handler=Handler}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#received_packet{packet_type=iq, type_attr=Type, raw_packet=IQ, from=From}, #state{handler=Handler}=State) ->
  	spawn(Handler, pre_process_iq, [Type, IQ, From, State]),
	{noreply, State};

handle_info({notify_channel, ID, User, Event, Time}, #state{handler=Handler}=State) ->
        spawn(Handler, notify_channel, [ID, User, Event, Time, State]),
        {noreply, State};

handle_info({_, tcp_closed}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
  ?INFO_MSG("Connection Closed. Trying to Reconnect...~n", []),
  {_, NewXmppCom} = make_connection(JID, Pass, Server, Port),
  ?INFO_MSG("Reconnected.~n", []),
  {noreply, State#state{xmppCom=NewXmppCom}};

handle_info({_,{bad_return_value, _}}, #state{jid=JID, server=Server, pass=Pass, port=Port}=State) ->
  ?INFO_MSG("Connection Closed. Trying to Reconnect...~n", []),
  {_, NewXmppCom} = make_connection(JID, Pass, Server, Port),
  ?INFO_MSG("Reconnected.~n", []),
  {noreply, State#state{xmppCom=NewXmppCom}};

handle_info(stop, #state{xmppCom=XmppCom}=State) ->
  ?INFO_MSG("Component Stopped.~n",[]),
  exmpp_component:stop(XmppCom),
  {noreply, State};

handle_info({get_active, _}=S,  #state{channelMonitor=ChannelMonitor}=State) ->
  ChannelMonitor ! S,
  {noreply, State};

handle_info(Record, State) -> 
  ?INFO_MSG("Unknown Info Request: ~p~n", [Record]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
 ?INFO_MSG("Received: ~p~n", [_Msg]), 
 {noreply, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Info,_From, _State) ->
 ?INFO_MSG("Received Call: ~p~n", [Info]), 
 {reply, ok, _State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{channelMonitor=ChannelMonitor}) ->
	?INFO_MSG("Terminating Component...", []),
	ChannelMonitor ! stop,
	application:stop(exmpp),
	?INFO_MSG("Terminated Component.", []),
	ok;

terminate(_Reason, _) -> 
	application:stop(exmpp),
        ?INFO_MSG("Forced Terminated Component.", []),
        ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

make_connection(JID, Pass, Server, Port) -> 
	XmppCom = exmpp_component:start(),
	make_connection(XmppCom, JID, Pass, Server, Port, 20).
make_connection(XmppCom, JID, Pass, Server, Port, 0) -> 
	exmpp_component:stop(XmppCom),
	make_connection(JID, Pass, Server, Port);
make_connection(XmppCom, JID, Pass, Server, Port, Tries) ->
    ?INFO_MSG("Connecting: ~p Tries Left~n",[Tries]),
    exmpp_component:auth(XmppCom, JID, Pass),
    try exmpp_component:connect(XmppCom, Server, Port) of
	R -> exmpp_component:handshake(XmppCom),
		?INFO_MSG("Connected.~n",[]),
		{R, XmppCom}
	catch
		Exception -> ?INFO_MSG("Exception: ~p~n",[Exception]),
		timer:sleep((20-Tries) * 200),
		make_connection(XmppCom, JID, Pass, Server, Port, Tries-1)
    end.

prepare_id([]) -> [];
prepare_id([$<|T]) -> [$x|prepare_id(T)];
prepare_id([$>|T]) -> [$X|prepare_id(T)];
prepare_id([H|T]) -> [H|prepare_id(T)].

unprepare_id([]) -> [];
unprepare_id([$x|T]) -> [$<|unprepare_id(T)];
unprepare_id([$X|T]) -> [$>|unprepare_id(T)];
unprepare_id([H|T]) -> [H|unprepare_id(T)].

is_allowed(_, []) -> true;
is_allowed({_,D,_}, WhiteDomain) ->
	is_allowed(D, WhiteDomain);
is_allowed(Domain, WhiteDomain) -> 
	lists:any(fun(S) -> S == Domain end, WhiteDomain).

get_port(PortMonitor) -> get_port(PortMonitor, 5).
get_port(_, 0) -> 
	?ERROR_MSG("Problem Retrieving Port Number",[]),
	{error, "Problem Retrieving Port Number"};
get_port(PortMonitor, T) ->
	PortMonitor ! {get_port, self()},
	receive
		{ok, Port} -> {ok, Port};
		{error, R} -> {error, R}
	after 200 -> get_port(PortMonitor, T-1) 
	end.

schedulePortMonitor(MinPort, MaxPort) -> spawn(fun () -> port_monitor(#port_mgr{minPort=MinPort, maxPort=MaxPort, port=MinPort}) end).

port_monitor(#port_mgr{}=Port) ->
	receive
		{get_port, PID} -> 
			{P, NewPort} = pull_port(Port),
		 	PID ! {ok, P},
			port_monitor(NewPort);
		stop ->
			?INFO_MSG("Stopping Port Manager",[]),
			ok;
		_ -> 
			?ERROR_MSG("Invalid Message Received by Port Monitor",[]),
			port_monitor(Port)
	end.	

pull_port(#port_mgr{minPort=InitPort, maxPort=EndPort, port=P}) when P > EndPort -> 
	pull_port(#port_mgr{minPort=InitPort, maxPort=EndPort, port=InitPort});
pull_port(#port_mgr{minPort=InitPort, maxPort=EndPort, port=P}) ->
	{P, #port_mgr{minPort=InitPort, maxPort=EndPort, port=P+4}}.

check_relay(#relay{pid= PID, user=U, id=ID, creationTime=CT}, Timeout) ->
	{TL, TR, NP} = gen_server:call(PID, get_timestamp),	
	DeltaL = timer:now_diff(now(), TL)/1000,
	UsedL =  timer:now_diff(TL, CT),
	DeltaR = timer:now_diff(now(), TR)/1000,
        UsedR =  timer:now_diff(TR, CT),
	Used = bigger(UsedL, UsedR),
	if
	DeltaL > Timeout orelse DeltaR > Timeout ->
		?INFO_MSG("Channel Killed: ~p Used for:~pms Processed:~p packets~n", [U, Used, NP]),
		exit(PID, kill),
		JnComp = whereis(jn_component),
	 	case is_pid(JnComp) of
			true ->
				JnComp ! {notify_channel, ID, U, killed, Used};
			_ -> ok
		end,
		removed;
	true -> 
		ok
	end.

bigger(A, A) -> A;
bigger(A, B) -> 
	case A > B of
		true -> A;
		_ -> B
	end.

check_relays(Relays, Timeout) ->
	check_relays(Relays, Timeout, []).

check_relays([], _, Remain) -> Remain;
check_relays([A|B], Timeout, Remain) ->
	case check_relay(A, Timeout) of
	ok -> check_relays(B, Timeout, [A|Remain]);
	_ -> check_relays(B, Timeout, Remain)
	end.

scheduleChannelPurge(Period, Relays, Timeout) -> spawn(fun () -> schedule(Period, Relays, Timeout) end).

get_stats() ->
	get_stats(3, whereis(jn_component)).
get_stats(0, _) -> -1;
get_stats(N, PID) ->
	PID!{get_active, self()},
	receive 
		{result_active, A} -> A
	after 100 -> get_stats(N-1, PID)
	end.

schedule(Period, Relays, Timeout) ->
    receive
        {get_active, PID} -> 
		Active = length(Relays),
		?INFO_MSG("Active Channels ~p~n", [Active]),
		PID!{result_active, Active},
		schedule(Period, Relays, Timeout);
	stop ->
                ?INFO_MSG("Stopping Schedule Loop.~n", []);
	NewRelay -> 
		?INFO_MSG("Relay Added: ~p~n", [NewRelay]),
		schedule(Period, [NewRelay | Relays], Timeout)
     after Period ->
	Remain = check_relays(Relays, Timeout),
        schedule(Period, Remain, Timeout)
    end.

get(_Key, []) ->
  ?ERROR_MSG("Property Not Found: ~p~n", [_Key]),
  not_found;
get(Key, [{Key, Value} | _Config]) ->
  Value;
get(Key, [{_Other, _Value} | Config]) ->
  get(Key, Config).
