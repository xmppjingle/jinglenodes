-module(jn_schedule).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").
-include("../include/jn_component.hrl").

%% API
-export([get_stats/0]).

%% gen_server callbacks
-export([start/2, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
	period :: integer(),
	relays = [] :: list(#relay{}), 
	timeout :: integer()
}).

start(Period, Timeout) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Period, Timeout], []).

get_stats() ->
    get_stats(3).
get_stats(0) -> 
	-1;
get_stats(N) ->
	case gen_server:call(?SERVER, get_active, 100) of
		timeout -> get_stats(N-1);
		{result_active, A} -> A
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

init([Period, Timeout]) ->
	{ok, #state{period=Period, timeout=Timeout}, Period}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(timeout, #state{relays=Relays,timeout=Timeout}=State) ->
    Remain = check_relays(Relays, Timeout),
    {noreply, State#state{relays=Remain}, Timeout};
handle_info(Record, State) -> 
    ?INFO_MSG("Unknown Info Request: ~p~n", [Record]),
    {noreply, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(NewRelay, #state{relays=Relays}=State) when is_record(NewRelay, relay) ->
	{noreply, State#state{relays=[NewRelay|Relays]}, State#state.timeout};
handle_cast(_Msg, State) ->
    ?INFO_MSG("Received: ~p~n", [_Msg]), 
    {noreply, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_active, _From, #state{relays=Relays}=State) ->
	Active = length(Relays),
	?INFO_MSG("Active Channels ~p~n", [Active]),
	{reply, {result_active, Active}, State#state.timeout};
handle_call(stop, _From, State) ->
	{stop, "Stopping Schedule Loop", ok, State};
handle_call(Info,_From, State) ->
    ?ERROR_MSG("Invalid Message Received by Port Monitor: ~p",[Info]),
    {reply, ok, State, State#state.timeout}.

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

check_relay(#relay{pid= PID, user=U, id=ID, creationTime=CT}, Timeout) ->
    {TL, TR, NP} = gen_server:call(PID, get_timestamp), 
    DeltaL = timer:now_diff(now(), TL)/1000,
    UsedL =  timer:now_diff(TL, CT),
    DeltaR = timer:now_diff(now(), TR)/1000,
    UsedR =  timer:now_diff(TR, CT),
    Used = trunc(max(UsedL, UsedR)/1000000),
    if
    DeltaL > Timeout orelse DeltaR > Timeout ->
        ?INFO_MSG("Channel Killed: ~p Used for:~ps Processed:~p packets~n", [U, Used, NP]),
        exit(PID, kill),
        jn_component ! {notify_channel, ID, U, killed, Used},
        false;
    true -> 
        true
    end.

check_relays(Relays, Timeout) ->
    ?INFO_MSG("Check relays: ~p~n", [Relays]),
	lists:filter(fun(R) ->
		check_relay(R, Timeout)
	end, Relays).
