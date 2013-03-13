-module(jn_portmonitor).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").
-include("../include/jn_component.hrl").

%% API
-export([get_port/0]).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link(MinPort, MaxPort) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MinPort, MaxPort], []).


-spec get_port() -> {ok, Port::integer()} | {error, Reason::string()}.

get_port() ->
	get_port(5).

-spec get_port(Timeout::integer()) -> {ok, Port::integer()} | {error, Reason::string()}.

get_port(0) -> 
    ?ERROR_MSG("Problem Retrieving Port Number",[]),
    {error, "Problem Retrieving Port Number"};
get_port(T) ->
	case gen_server:call(?SERVER, get_port, 200) of
		timeout -> get_port(T-1);
		Any -> Any
	end.

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

init([MinPort, MaxPort]) ->
	{ok, #port_mgr{minPort=MinPort, maxPort=MaxPort, port=MinPort}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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
handle_call(get_port, _From, Port) ->
	{P, NewPort} = pull_port(Port),
	{reply, {ok, P}, NewPort};
handle_call(stop, _From, Port) ->
	{stop, normal, ok, Port};
handle_call(Info,_From, _State) ->
    ?ERROR_MSG("Invalid Message Received by Port Monitor: ~p",[Info]),
    {reply, ok, _State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _) -> 
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

pull_port(#port_mgr{minPort=InitPort, maxPort=EndPort, port=P}) when P > EndPort -> 
    pull_port(#port_mgr{minPort=InitPort, maxPort=EndPort, port=InitPort});
pull_port(#port_mgr{minPort=InitPort, maxPort=EndPort, port=P}) ->
    {P, #port_mgr{minPort=InitPort, maxPort=EndPort, port=P+4}}.
