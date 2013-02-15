%%%-------------------------------------------------------------------
%%% File    : jn_component.erl
%%% Author  : Thiago Camargo <barata7@gmail.com>
%%% Description : Jingle Nodes Services - External Component
%%% Provides:
%%%     * UDP Relay Services
%%%
%%% Created : 01 Nov 2009 by Thiago Camargo <barata7@gmail.com>
%%% Updated : 30 Jan 2013 by Manuel Rubio <bombadil@bosqueviejo.net>
%%%-------------------------------------------------------------------

-module(jn_component).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").
-include("../include/jn_component.hrl").

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(State) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

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

init(State) when is_record(State, jnstate) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({iq,#params{type=Type}=Params}, #jnstate{handler=Handler}=State) ->
    spawn(Handler, process_iq, [Type, Params, State]),
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
handle_cast({notify_channel, ID, User, Event, Time}, #jnstate{handler=Handler}=State) ->
    spawn(Handler, notify_channel, [ID, User, Event, Time, State]),
    {noreply, State};

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
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Info,_From, State) ->
    ?INFO_MSG("Received Call: ~p~n", [Info]), 
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _) -> 
    gen_server:call(jn_schedule, stop),
    gen_server:call(jn_portmonitor, stop),
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
