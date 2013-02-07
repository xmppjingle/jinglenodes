-define(INFO_MSG(M, P), lager:info(M, P)).
-define(ERROR_MSG(M, P), lager:error(M, P)).
-define(NS_CHANNEL,'http://jabber.org/protocol/jinglenodes#channel').
-define(NAME_CHANNEL,'channel').
-define(NS_CHANNEL_REDIRECT,'http://jabber.org/protocol/jinglenodes#channelredirect').
-define(NS_JINGLE_NODES_s,"http://jabber.org/protocol/jinglenodes").
-define(NS_JINGLE_NODES,'http://jabber.org/protocol/jinglenodes').
-define(NS_JINGLE_NODES_EVENT, 'http://jabber.org/protocol/jinglenodes#event').
-define(NAME_SERVICES,'services').
-define(NS_CHANNEL_s,"http://jabber.org/protocol/jinglenodes#channel").
 
-record(relay, {pid, user, id, creationTime}).
-record(jn_relay_service, {address, xml}).
-record(jn_tracker_service, {address, xml}).
-record(port_mgr, {minPort, maxPort, port}).
-record(jnstate, {
	xmppCom, 
	jid, 
	pass, 
	server, 
	port, 
	pubIP, 
	whiteDomain, 
	maxPerPeriod, 
	periodSeconds, 
	handler, 
	broadcast,
	channelTimeout :: integer(),
	initPort :: integer(),
	endPort :: integer()
}).
