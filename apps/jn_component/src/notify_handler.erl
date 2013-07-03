-module(notify_handler).

-define(NS_CALL_EVENT, 'achievement#event').

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include("../include/jn_component.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").

-export([notify_channel/5]).

notify_channel(_ID, _JID, _Event, _Time, undefined) ->
	undefined;
notify_channel(_ID, {_Node, _, _}, Event, Time, BJID) ->
	NEvent = choose_event(Event),
    Notify = exmpp_xml:element(?NS_CALL_EVENT, 'query', [],[
    	exmpp_xml:element(undefined, 'event', [
    		exmpp_xml:attribute(<<"key">>, NEvent), 
    		exmpp_xml:attribute(<<"value">>, Time)
    	], [])
    ]),
    SetBare = exmpp_iq:set(?NS_COMPONENT_ACCEPT, Notify),
    exmpp_xml:set_attribute(SetBare, <<"to">>, BJID);
notify_channel(_, _, _, _, _) ->
	undefined.

choose_event(killed) -> 'call_killed';
choose_event(_) -> undefined.
