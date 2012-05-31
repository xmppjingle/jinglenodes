%%%-------------------------------------------------------------------
%%% File    : mod_monitor.erl
%%% Author  : Thiago Camargo <barata7@gmail.com>
%%% Description : Generic Erlang/Mnesia Throttle
%%% Provides:
%%%             * Throttle Function based on Max Requests for an Interval for an ID(node)
%%%
%%% Created : 16 Apr 2010  by Thiago Camargo <barata7@gmail.com>
%%%-------------------------------------------------------------------

-module(mod_monitor).

-define(ERROR_MSG(Format, Args),
        error_logger:error_msg("(~p:~p:~p) " ++ Format ++ "~n",
                               [self(), ?MODULE, ?LINE | Args])).

-define(INFO_MSG(Format, Args),
        error_logger:info_msg("(~p:~p:~p) " ++ Format ++ "~n",
                               [self(), ?MODULE, ?LINE | Args])).

-export([init/0,
	accept/3]).

-record(monitor, {id, counter, timestamp}).

init() ->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    mnesia:create_table(monitor,
                        [{attributes, record_info(fields, monitor)}]).

accept(N=#monitor{}, C, Max) ->
        update_node(N, now(), C),
        if C > Max -> 
                false;
        true ->
                true
        end;

accept(Id, Max, Period) ->
        N = get_node(Id),
        case N of
        {'EXIT', _Reason} ->
                false;
        _ ->
                Timestamp=N#monitor.timestamp,
        	Counter=N#monitor.counter+1,
                D = to_mile(timer:now_diff(now(),Timestamp)),
		if D > Period ->
                        NC = reset_counter(D, Counter, Max, Period),
                        ?INFO_MSG("Monitor Counter Updated: from ~p to ~p", [Counter, NC]),
			accept(N, NC, Max);
                true ->
                        accept(N, Counter, Max)
                end
	end.

to_mile(T) -> T/1000000.

reset_counter(Time_delta, Counter, Max, Period) ->
	C = Counter - Max * trunc(Time_delta/Period),
	if C < 0 -> 0;	true -> C end.

update_node(N, T, C) ->
	NN = #monitor{id=N#monitor.id, counter=C, timestamp=T},
	case mnesia:dirty_write(monitor,NN) of
        {'EXIT', _Reason} ->
                ?ERROR_MSG("Found no session for ~s",[id]);
        _ -> 
		NN
	end.

get_node(Id) ->
	V = mnesia:dirty_read(monitor, Id),
	case V of
	{'EXIT', _Reason} ->
		add_node(Id);
	[] -> 
		add_node(Id);
	[N|_] -> N
	end.

add_node(Node_id) ->
	N = #monitor{id=Node_id, counter=0, timestamp = now()},
	case mnesia:dirty_write(monitor,N) of
	{'EXIT', _Reason} ->
		?ERROR_MSG("Found no session for ~s",[id]);
	_ -> N
	end.
	
