-module(jingle_handler).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/jn_component.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").

%% API
-export([notify_channel/5, allocate_relay/1, process_iq/3]).

notify_channel(ID, {Node, Domain, Resource}=JID, Event, Time, #jnstate{broadcast=BJID, jid=CJID}=State) ->
    Notify = exmpp_xml:element(?NS_JINGLE_NODES_EVENT, 'channel', [exmpp_xml:attribute(<<"event">>, Event), exmpp_xml:attribute(<<"id">>, ID), exmpp_xml:attribute(<<"time">>, integer_to_list(Time))], []),
        SetBare = exmpp_iq:set(?NS_COMPONENT_ACCEPT, Notify),
    case Node of
        undefined ->
            From = CJID;
        _ ->
            From =  erlang:binary_to_list(Node) ++ "@" ++ CJID
    end,
    SetTo = exmpp_xml:set_attribute(SetBare, <<"to">>, exmpp_jid:to_list(Node, Domain, Resource)),  
        ecomponent:send(SetTo, ?MODULE),
    Broadcast = erlang:apply(notify_handler, notify_channel, [ID, JID, Event, Time, BJID]),
    case Broadcast of
        undefined ->
            ok;
        _ ->
            BroadcastFrom = exmpp_xml:set_attribute(Broadcast, <<"from">>, From),
            ecomponent:send(BroadcastFrom, ?MODULE)
    end,
        {ok, State};
notify_channel(_, _, _, _, #jnstate{}=State)-> {ok, State}.


-spec is_allowed( Domain::string(), List::list(binary()) ) -> boolean().

is_allowed(Domain, List) ->
    lists:member(Domain, List).


%% Create Channel and return details
process_iq("get", #params{from=From, ns=?NS_CHANNEL, iq=IQ}, #jnstate{pubIP=PubIP, whiteDomain=WhiteDomain}=State) ->
    Permitted = is_allowed(From, WhiteDomain), 
    if Permitted =:= true ->
        case allocate_relay(From) of
            {ok, PortA, PortB, ID} ->
                ?INFO_MSG("Allocated Port for: ~p ~p~n", [From, ID]),
                Result = exmpp_iq:result(IQ ,get_candidate_elem(PubIP, PortA, PortB, ID)),
                ecomponent:send(Result, ?MODULE),
                {ok, State};
            _ ->
                ?ERROR_MSG("Could Not Allocate Port for : ~p~n", [From]),
                Error = exmpp_iq:error_without_original(IQ, 'internal-server-error'),
                ecomponent:send(Error, ?MODULE),
                {error, State}
        end;
    true -> 
        ?ERROR_MSG("[Not Acceptable] Could Not Allocate Port for : ~p on ~p~n", [From, WhiteDomain]),
        Error = exmpp_iq:error_without_original(IQ, 'policy-violation'),
                ecomponent:send(Error, ?MODULE),
        {error, State}      
    end;

process_iq("get", #params{ns=?NS_DISCO_INFO, iq=IQ}, #jnstate{}=State) ->
    Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_DISCO_INFO, 'query', [], [
        exmpp_xml:element(?NS_DISCO_INFO, 'identity', [
            exmpp_xml:attribute("category", <<"proxy">>),
            exmpp_xml:attribute("type", <<"relay">>),
            exmpp_xml:attribute("name", <<"Jingle Nodes Relay">>)
        ], []), 
        exmpp_xml:element(?NS_DISCO_INFO, 'feature', [
            exmpp_xml:attribute(<<"var">>, ?NS_JINGLE_NODES_s)
        ],[]), 
        exmpp_xml:element(?NS_DISCO_INFO, 'feature', [
            exmpp_xml:attribute(<<"var">>, ?NS_CHANNEL_s)
        ],[])
    ])),
    ecomponent:send(Result, ?MODULE),
    {ok, State};

process_iq("get", #params{ns=?NS_JINGLE_NODES, iq=IQ}, #jnstate{jid=JID}=State) ->
    Result = exmpp_iq:result(IQ, exmpp_xml:element(?NS_JINGLE_NODES, ?NAME_SERVICES, [],[
        exmpp_xml:element(undefined, 'relay', [
            exmpp_xml:attribute(<<"policy">>,"public"), 
            exmpp_xml:attribute(<<"protocol">>, "udp"), 
            exmpp_xml:attribute(<<"address">>, JID)
        ], [])
    ])),
    ecomponent:send(Result, ?MODULE),
    {ok, State};

process_iq("get", #params{ns=?NS_PING, iq=IQ}, #jnstate{}=State) ->
    ecomponent:send(exmpp_iq:result(IQ), ?MODULE),
    {ok, State};

process_iq("set", #params{ns=?NS_CHANNEL_REDIRECT, payload=Payload, iq=IQ}, #jnstate{}=State) ->
    ID = exmpp_xml:get_attribute(Payload, <<"id">>, ""),
    process_redirect(Payload, ID),
    ecomponent:send(exmpp_iq:result(IQ), ?MODULE),
    {ok, State};

process_iq(_, P, #jnstate{}=State) ->
    ?INFO_MSG("Unknown Request: ~p~n", [P]),        
    {ok, State}.

process_redirect(Payload, PID) when erlang:is_pid(PID) ->
    call_redirect(Payload, PID);
process_redirect(Payload, ID) when erlang:is_binary(ID) ->
    process_redirect(Payload, erlang:binary_to_list(ID));
process_redirect(Payload, IDstr) ->
    try 
        ID = ecomponent:unprepare_id(IDstr),
        PID = erlang:list_to_pid(ID),
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
    Username=exmpp_xml:get_attribute(H, <<"username">>,<<"jingnode">>),
    Host=exmpp_xml:get_attribute(H,<<"host">>,null),
    Port=exmpp_xml:get_attribute(H,<<"port">>,null),
    ?INFO_MSG("Call Redirect: ~p ~p ~p ~p~n", [Username, Host, Port, PID]),
    call_redirect(Username, Host, Port, PID).

call_redirect(_, null, _, _) -> ok;
call_redirect(_, _, null, _) -> ok;
call_redirect(Username, Host, Port, PID) ->
    PID ! {redirect_remote, Username, Host, Port}.

get_candidate_elem(Host, A, B, ID) ->
    Raw_Elem = exmpp_xml:element(?NS_CHANNEL,?NAME_CHANNEL),
    Elem_A = exmpp_xml:set_attribute(Raw_Elem, <<"localport">>, A),
    Elem_B = exmpp_xml:set_attribute(Elem_A, <<"remoteport">>, B),
    Elem_C = exmpp_xml:set_attribute(Elem_B, <<"id">>, ecomponent:prepare_id(ID)),
    exmpp_xml:set_attribute(Elem_C, <<"host">>, Host).

allocate_relay(U) -> 
    allocate_relay(U, 5).
allocate_relay(U, 0) -> 
    ?ERROR_MSG("Could Not Allocate Port for : ~p~n", [U]),
    {error, -1, -1};
allocate_relay(U, Tries) ->
    case jn_portmonitor:get_port() of
        {ok, Port} ->
            PortB = Port + 2,
            CT = now(),
            case jingle_relay:start(Port, PortB) of
                {ok, R} ->
                    ID = erlang:pid_to_list(R), 
                    gen_server:cast(jn_portmonitor, #relay{pid=R, user=U, id=ID, creationTime=CT}),
                    {ok, Port, PortB, ID};
                _ -> 
                    allocate_relay(U, Tries-1)
            end;
        {error, M} ->
            ?ERROR_MSG("Could Not Allocate Port for : ~p ~p~n", [U, M]),
            {error, -1, -1}
    end.
