-module(jingle_handler).

-define(NS_CHANNEL,'http://jabber.org/protocol/jinglenodes#channel').
-define(NAME_CHANNEL,'channel').
-define(NS_CHANNEL_REDIRECT,'http://jabber.org/protocol/jinglenodes#channelredirect').
-define(NS_JINGLE_NODES_s,"http://jabber.org/protocol/jinglenodes").
-define(NS_JINGLE_NODES,'http://jabber.org/protocol/jinglenodes').
-define(NS_JINGLE_NODES_EVENT, 'http://jabber.org/protocol/jinglenodes#event').
-define(NAME_SERVICES,'services').
-define(NS_CHANNEL_s,"http://jabber.org/protocol/jinglenodes#channel").
-define(SERVER, ?MODULE).

-import(config).
-import(file).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/jn_component.hrl").

%% API
-export([pre_process_iq/4]).
-export([notify_channel/5]).

notify_channel(ID, {Node, Domain, Resource}, Event, Time, #state{jid=JID, xmppCom=XmppCom}=State) ->
        ?INFO_MSG("Notify Details: ~p ~p ~p ~p~n", [ID, exmpp_jid:to_list(Node, Domain, Resource), Event, JID]),
	Notify = exmpp_xml:element(?NS_JINGLE_NODES_EVENT, 'channel', [exmpp_xml:attribute('event', Event), exmpp_xml:attribute('id', jn_component:prepare_id(ID)), exmpp_xml:attribute('time', integer_to_list(Time))], []),
        SetBare = exmpp_iq:set(?NS_COMPONENT_ACCEPT, Notify),
	SetTo = exmpp_xml:set_attribute(SetBare, to, exmpp_jid:to_list(Node, Domain, Resource)),	
	SetFrom = exmpp_xml:set_attribute(SetTo, from, JID),
        exmpp_component:send_packet(XmppCom, SetFrom),
	?INFO_MSG("Notify Sent: ~p ~n", [SetFrom]),
        {ok, State};
notify_channel(_, _, _, _, #state{}=State)-> {ok, State}.

pre_process_iq(Type, IQ, From, State) ->
        ?INFO_MSG("Preparing: ~p~n On State:~p~n", [IQ, State]),
        Payload = exmpp_iq:get_payload(IQ),
        NS = exmpp_xml:get_ns_as_atom(Payload),
        ?INFO_MSG("NS:~p~n", [NS]),
        process_iq(Type, IQ, From, NS, Payload, State).

%% Create Channel and return details
process_iq("get", IQ, From, ?NS_CHANNEL, _, #state{xmppCom=XmppCom, pubIP=PubIP, channelMonitor=ChannelMonitor, whiteDomain=WhiteDomain, maxPerPeriod=MaxPerPeriod, periodSeconds=PeriodSeconds, portMonitor=PortMonitor}=State) ->
    Permitted = jn_component:is_allowed(From, WhiteDomain) andalso mod_monitor:accept(From, MaxPerPeriod, PeriodSeconds),	
	if Permitted == true ->
		?INFO_MSG("T: ~p~n", [PortMonitor]),
    		case allocate_relay(ChannelMonitor, From, PortMonitor) of
		{ok, PortA, PortB, ID} ->
			?INFO_MSG("Allocated Port for : ~p ~p~n", [From, ID]),
			Result = exmpp_iq:result(IQ,get_candidate_elem(PubIP, PortA, PortB, ID)),
			exmpp_component:send_packet(XmppCom, Result),
			{ok, State};
		_ ->
			?ERROR_MSG("Could Not Allocate Port for : ~p~n", [From]),
			Error = exmpp_iq:error_without_original(IQ, 'internal-server-error'),
			exmpp_component:send_packet(XmppCom, Error),
			{error, State}
		end;
	true -> 
		?ERROR_MSG("[Not Acceptable] Could Not Allocate Port for : ~p~n", [From]),
		Error = exmpp_iq:error_without_original(IQ, 'policy-violation'),
                exmpp_component:send_packet(XmppCom, Error),
		{error, State}		
	end;

process_iq("get", IQ, _, ?NS_DISCO_INFO, _, #state{xmppCom=XmppCom}=State) ->
        Identity = exmpp_xml:element(?NS_DISCO_INFO, 'identity', [exmpp_xml:attribute("category", <<"proxy">>),
                                                      exmpp_xml:attribute("type", <<"relay">>),
                                                      exmpp_xml:attribute("name", <<"Jingle Nodes Relay">>)
                                                      ],
                                     []),
        IQRegisterFeature1 = exmpp_xml:element(?NS_DISCO_INFO, 'feature', [exmpp_xml:attribute('var', ?NS_JINGLE_NODES_s)],[]),
        IQRegisterFeature2 = exmpp_xml:element(?NS_DISCO_INFO, 'feature', [exmpp_xml:attribute('var', ?NS_CHANNEL_s)],[]),
        Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_DISCO_INFO, 'query', [], [Identity, IQRegisterFeature1, IQRegisterFeature2])),
        exmpp_component:send_packet(XmppCom, Result),
	{ok, State};

process_iq("get", IQ, _, ?NS_JINGLE_NODES, _, #state{jid=JID, xmppCom=XmppCom}=State) ->
	Relay = exmpp_xml:element(undefined, 'relay', [exmpp_xml:attribute('policy',"public"), exmpp_xml:attribute('protocol', "udp"), exmpp_xml:attribute('address', JID)], []),
	Services = exmpp_xml:element(?NS_JINGLE_NODES, ?NAME_SERVICES, [],[Relay]),
	Result = exmpp_iq:result(IQ, Services),
	exmpp_component:send_packet(XmppCom, Result),
	{ok, State};

process_iq("get", IQ, _, ?NS_PING, _, #state{xmppCom=XmppCom}=State) ->
        Result = exmpp_iq:result(IQ),
        exmpp_component:send_packet(XmppCom, Result),
        {ok, State};

process_iq("set", IQ, _, ?NS_CHANNEL_REDIRECT, Payload, #state{xmppCom=XmppCom}=State) ->
        ID=exmpp_xml:get_attribute(Payload, "id", ""),
        process_redirect(Payload, ID),
        Result = exmpp_iq:result(IQ),
        exmpp_component:send_packet(XmppCom, Result),
        {ok, State};

process_iq(_, IQ, _, _, _, #state{}=State) ->
	?INFO_MSG("Unknown Request: ~p~n", [IQ]),	    
	{ok, State}.

process_redirect(Payload, PID) when erlang:is_pid(PID) ->
        call_redirect(Payload, PID);
process_redirect(Payload, ID) when erlang:is_binary(ID) ->
        process_redirect(Payload, erlang:binary_to_list(ID));
process_redirect(Payload, IDstr) ->
        try	
		?INFO_MSG("P Redirect IDstr: ~p [~p]~n", [IDstr, Payload]),	
		ID = jn_component:unprepare_id(IDstr),
		?INFO_MSG("P Redirect ID: [~p]~n", [ID]),
		PID = erlang:list_to_pid(ID),
                ?INFO_MSG("P Redirect ID: [~p]~n", [ID]),
		process_redirect(Payload, PID)
        catch
                E:_R ->
                ?ERROR_MSG("Invalid Channel ID: ~p [~p]~n", [IDstr, E]),
                error
        end.

call_redirect([], _) ->
	?INFO_MSG("Call Redirect BLANK~n", []), 
	ok;
call_redirect(H, PID) ->
        ?INFO_MSG("Call Redirect: ~p~n", [H]),
        Username=exmpp_xml:get_attribute(H,"username",<<"jingnode">>),
        Host=exmpp_xml:get_attribute(H,"host",null),
        Port=exmpp_xml:get_attribute(H,"port",null),
	?INFO_MSG("Call Redirect: ~p ~p ~p ~p~n", [Username, Host, Port, PID]),
	call_redirect(Username, Host, Port, PID).

call_redirect(_, null, _, _) -> ok;
call_redirect(_, _, null, _) -> ok;
call_redirect(Username, Host, Port, PID) ->
	%gen_server:call(PID, {redirect_remote, Username, Host, Port}).
	PID ! {redirect_remote, Username, Host, Port}.

get_candidate_elem(Host, A, B, ID) ->
	Raw_Elem = exmpp_xml:element(?NS_CHANNEL,?NAME_CHANNEL),
        Elem_A = exmpp_xml:set_attribute(Raw_Elem, "localport", A),
        Elem_B = exmpp_xml:set_attribute(Elem_A, "remoteport", B),
	Elem_C = exmpp_xml:set_attribute(Elem_B, "id", jn_component:prepare_id(ID)),
        exmpp_xml:set_attribute(Elem_C, "host", Host).

allocate_relay(ChannelMonitor, U, PortMonitor) -> allocate_relay(ChannelMonitor, U, 5, PortMonitor).
allocate_relay(_, U, 0, _) -> 
	 ?ERROR_MSG("Could Not Allocate Port for : ~p~n", [U]),
	{error, -1, -1};
allocate_relay(ChannelMonitor, U, Tries, PortMonitor) ->
     	case jn_component:get_port(PortMonitor) of
		{ok, Port} ->
     			PortB = Port + 2,
			CT = now(),
     			case jingle_relay:start(Port, PortB) of
				{ok, R} ->
					ID=erlang:pid_to_list(R), 
					ChannelMonitor ! #relay{pid=R, user=U, id=ID, creationTime=CT},
					{ok, Port, PortB, ID};
				_ -> allocate_relay(ChannelMonitor, U, Tries-1, PortMonitor)
	     		end;
		{error, M} ->
			?ERROR_MSG("Could Not Allocate Port for : ~p ~p~n", [U, M]),
        		{error, -1, -1}
	end.
