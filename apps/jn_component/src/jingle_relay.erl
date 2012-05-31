%%%-------------------------------------------------------------------
%%% File    : jingle_relay.erl
%%% Author  : Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Author  : Thiago <barata7@gmail.com>
%%%
%%% Description : Simple UDP relay with RTCP Port Support
%%%
%%% Created : 29 Oct 2009 by Evgeniy Khramtsov <ekhramtsov@process-one.net>
%%% Update : 17 Dec 2009 by Thiago <barata7@gmail.com>
%%%-------------------------------------------------------------------
-module(jingle_relay).

-behaviour(gen_server).

-define(INFO_MSG(M, P), lager:info(M, P)).
-define(ERROR_MSG(M, P), lager:error(M, P)).

%% API
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {local_sock, remote_sock, last_recv_local, last_recv_remote, local_sock_c, remote_sock_c, last_recv_local_c, last_recv_remote_c, lastTimestamp_local, lastTimestamp_remote, npackets}).

-define(SOCKOPTS, [binary, {active, once}]).

%%====================================================================
%% API
%%====================================================================
start_link(P1, P2) ->
    gen_server:start_link(?MODULE, [P1, P2], []).

start(P1, P2) ->
    gen_server:start(?MODULE, [P1, P2], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Port1, Port2]) ->
	init([Port1, Port2], 5).
init([Port1, Port2], 0) -> 
	?ERROR_MSG("unable to open port: ~p ~p", [Port1, Port2]),
        {stop};
init([Port1, Port2], T) ->
    case {gen_udp:open(Port1, ?SOCKOPTS),
	  gen_udp:open(Port1+1, ?SOCKOPTS),
	  gen_udp:open(Port2, ?SOCKOPTS),
	  gen_udp:open(Port2+1, ?SOCKOPTS)} of
	{{ok, Local_Sock}, {ok, Local_Sock_C}, {ok, Remote_Sock}, {ok, Remote_Sock_C}} ->
	    ?INFO_MSG("relay started at ~p and ~p", [Port1, Port2]),
	    {ok, #state{local_sock = Local_Sock, local_sock_c = Local_Sock_C, remote_sock = Remote_Sock, remote_sock_c = Remote_Sock_C, lastTimestamp_local = now(), lastTimestamp_remote = now(), npackets=0}};
	Errs ->
	    ?ERROR_MSG("unable to open port: ~p", [Errs]),
	    init([Port1, Port2], T-1)
    end.

handle_call(get_timestamp, _From, State) ->
    {reply, {State#state.lastTimestamp_local, State#state.lastTimestamp_remote, State#state.npackets}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Sock, SrcIP, SrcPort, Data},
	    #state{local_sock = Sock, npackets=NPackets} = State) ->
    inet:setopts(Sock, [{active, once}]),
	case State#state.last_recv_remote of
			{DstIP, DstPort} ->
	    			send(State#state.remote_sock, DstIP, DstPort, Data),
				{noreply, State#state{last_recv_local = {SrcIP, SrcPort}, lastTimestamp_local= now(), npackets=NPackets+1}};
			_ ->
				{noreply, State#state{last_recv_local = {SrcIP, SrcPort}, lastTimestamp_local= now(), npackets=NPackets+1}}
    	end;

handle_info({udp, Sock, SrcIP, SrcPort, Data},
	    #state{remote_sock = Sock, npackets=NPackets} = State) ->
    inet:setopts(Sock, [{active, once}]),
	case State#state.last_recv_local of
		{DstIP, DstPort} ->
	   		send(State#state.local_sock, DstIP, DstPort, Data),
			{noreply, State#state{last_recv_remote = {SrcIP, SrcPort}, lastTimestamp_remote = now(), npackets=NPackets+1}};
		_ ->
		        {noreply, State#state{last_recv_remote = {SrcIP, SrcPort}, lastTimestamp_remote = now(), npackets=NPackets+1}}
    	end;

handle_info({udp, Sock, SrcIP, SrcPort, Data},
	    #state{local_sock_c = Sock} = State) ->
    inet:setopts(Sock, [{active, once}]),  
        case State#state.last_recv_remote_c of
	{DstIP, DstPort} ->
	    send(State#state.remote_sock_c, DstIP, DstPort, Data);
	_ ->
	    ok
        end,
        {noreply, State#state{last_recv_local_c = {SrcIP, SrcPort}, lastTimestamp_local = now()}};

handle_info({udp, Sock, SrcIP, SrcPort, Data},
	    #state{remote_sock_c = Sock} = State) ->
    inet:setopts(Sock, [{active, once}]),
    	case State#state.last_recv_local_c of
		{DstIP, DstPort} ->
	    	send(State#state.local_sock_c, DstIP, DstPort, Data);
		_ ->
		ok
    	end,
        {noreply, State#state{last_recv_remote_c = {SrcIP, SrcPort}, lastTimestamp_remote= now()}};

handle_info({redirect_remote, Username, Host, Port}, #state{remote_sock = Sock, remote_sock_c= _Sock_c} = State) ->
    IPort = list_to_integer(binary_to_list(Port)),
    {ok, IHost} = inet_parse:address(binary_to_list(Host)),
    SR = <<0,1,36:16,IPort:128,6:16,32:16, Username/binary>>,
    send(Sock, IHost, IPort, SR), 
    {noreply, State};

handle_info(_Info, State) ->
	?INFO_MSG("Unknown Info: ~p ~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send(Sock, Addr, Port, Data) ->
    case gen_udp:send(Sock, Addr, Port, Data) of
	ok ->
	    ok;
	Err ->
	    ?ERROR_MSG("unable to send data: ~p", [Err]),
	    exit(normal)
    end.
