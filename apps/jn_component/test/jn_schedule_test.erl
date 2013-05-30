-module(jn_schedule_test).
-compile([export_all]).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").
-include("../include/jn_component.hrl").

setup_test_() ->
	{foreach, 
		fun init_per_testcase/0,
		fun end_per_testcase/1,
		[
			fun init_and_stop/1,
			fun add_relays/1,
			fun stats_test/1
		]
	}.

init_per_testcase() ->
	meck:new(folsom_metrics),
	meck:expect(folsom_metrics, new_gauge, fun(_) -> ok end),
	meck:expect(folsom_metrics, notify, fun(_) -> ok end),  

	case ets:info(metrics) of 
		undefined -> 
			ets:new(metrics, [named_table, public]);
		_ ->
			ok
	end,
	jn_schedule:start_link(1, 10),
	ok.

end_per_testcase(_Config) ->
	gen_server:call(jn_schedule, stop),
	meck:unload(folsom_metrics),
	timer:sleep(500),
	ok.

init_and_stop(_Config) ->
	?assertNotEqual(undefined, erlang:whereis(jn_schedule)),
	gen_server:call(jn_schedule, stop),
	timer:sleep(500),
	?assertEqual(undefined, erlang:whereis(jn_schedule)),
	jn_schedule:start_link(1,10),
	?_assertNotEqual(undefined, erlang:whereis(jn_schedule)).

add_relays(_Config) ->
	register(jn_component, self()),
	gen_server:cast(jn_schedule, #relay{
		pid=self(),
		creationTime=now()
	}),
	gen_server:cast(jn_schedule, #relay{
		pid=self(),
		creationTime=now()
	}),
	%% get timestamp from relay
	receive
		{'$gen_call',From1,get_timestamp} -> 
			gen_server:reply(From1, {{1,1,1}, {1,1,1}, 0});
		Any0 -> 
			throw(Any0)
	after 2000 ->
		throw("Timeout!")
	end,
	%% stop the relay
	receive
		{'$gen_cast', stop} -> ok;
		Any2 -> throw(Any2)
	after 2000 ->
		throw("Timeout!")
	end,
	%% notify channel
	receive
		{'$gen_cast',{notify_channel,_ID,_User,killed,_Used}} -> ok;
		Any1 -> throw(Any1)
	after 5000 ->
		throw("Timeout!")
	end,
	%% get next timestamp from relay
	receive
		{'$gen_call',From2,get_timestamp} -> 
			gen_server:reply(From2, {now(), now(), 0});
		Any3 -> 
			throw(Any3)
	after 2000 ->
		throw("Timeout!")
	end,
	?_assert(unregister(jn_component)).

stats_test(_Config) ->
	?_assertEqual(0, jn_schedule:get_stats()).
