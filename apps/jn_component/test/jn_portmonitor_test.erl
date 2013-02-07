-module(jn_portmonitor_test).
-compile([export_all]).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
	{setup, 
		fun init_per_suite/0,
		fun end_per_suite/1,
		fun (Config) -> [
			init_and_stop(Config),
			get_ports(Config)
		] end
	}.

init_per_suite() ->
	jn_portmonitor:start_link(1,10),
	ok.

end_per_suite(_Config) ->
	gen_server:call(jn_portmonitor, stop),
	ok.

init_and_stop(_Config) ->
	?assertNotEqual(undefined, erlang:whereis(jn_portmonitor)),
	gen_server:call(jn_portmonitor, stop),
	timer:sleep(500),
	?assertEqual(undefined, erlang:whereis(jn_portmonitor)),
	jn_portmonitor:start_link(1,10),
	?_assertNotEqual(undefined, erlang:whereis(jn_portmonitor)).

get_ports(_Config) ->
	?assertEqual({ok, 1}, jn_portmonitor:get_port()),
	?assertEqual({ok, 5}, jn_portmonitor:get_port()),
	?assertEqual({ok, 9}, jn_portmonitor:get_port()),
	?assertEqual({ok, 1}, jn_portmonitor:get_port()),
	?assertEqual({ok, 5}, jn_portmonitor:get_port()),
	?_assertEqual({ok, 9}, jn_portmonitor:get_port()).
