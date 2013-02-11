-module(jingle_relay_test).
-compile([export_all]).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").


setup_test_() -> [
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
        	get_timestamp_test(Config),
        	send_messages_test(Config),
        	redirect_remote_test(Config)
        ] end
    },
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
        	get_timestamp_test(Config),
        	send_rtpc_test(Config)
        ] end
    },
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
        	get_timestamp_test(Config),
        	inverse_send_messages_test(Config)
        ] end
    },
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
        	get_timestamp_test(Config),
        	inverse_send_rtpc_test(Config)
        ] end
    }
].

init_per_suite() ->
	Pid = self(),
	meck:new(gen_udp, [unstick]),
	meck:expect(gen_udp, open, fun(Port, _Opts) ->
		Pid ! Port,
		{ok, Port}
	end),
	meck:new(inet, [unstick]),
	meck:expect(inet, setopts, fun(_Sock, _Opts) ->
		ok
	end),
	{ok, JingleRelayPid} = jingle_relay:start(0, 2),
	?assertEqual(ok, receive 0 -> ok; A -> throw(A) end),
	?assertEqual(ok, receive 1 -> ok; A -> throw(A) end),
	?assertEqual(ok, receive 2 -> ok; A -> throw(A) end),
	?assertEqual(ok, receive 3 -> ok; A -> throw(A) end),
	JingleRelayPid.

end_per_suite(Pid) ->
	meck:unload(gen_udp),
	meck:unload(inet),
	gen_server:cast(Pid, stop).

get_timestamp_test(Pid) ->
	[
		?_assertMatch({_MS,_S,_NS}, gen_server:call(Pid, get_timestamp))
	].

send_messages_test(Pid) ->
	[
		%% local sends a packet... but destin is unknown
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 0, srcip, srcport, "a"},
			receive "a" -> throw("Sended?!?") after 500 -> true end
		end),

		%% remote sends a packet and is relayed to local
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 2, dstip, dstport, "b"},
			receive "b" -> true after 500 -> throw("Timeout!") end
		end),

		%% local sends a packet and is relayed to remote
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 0, srcip, srcport, "c"},
			receive "c" -> true after 500 -> throw("Timeout!") end
		end)
	].

send_rtpc_test(Pid) ->
	[
		%% local sends a packet... but destin is unknown
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 1, srcip, srcport, "a"},
			receive "a" -> throw("Sended?!?") after 500 -> true end
		end),

		%% remote sends a packet and is relayed to local
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 3, dstip, dstport, "b"},
			receive "b" -> true after 500 -> throw("Timeout!") end
		end),

		%% local sends a packet and is relayed to remote
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 1, srcip, srcport, "c"},
			receive "c" -> true after 500 -> throw("Timeout!") end
		end)
	].

inverse_send_messages_test(Pid) ->
	[
		%% remote sends a packet... but destin is unknown
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 2, srcip, srcport, "b"},
			receive "b" -> throw("Sended?!?") after 500 -> true end
		end),

		%% local sends a packet and is relayed to remote
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 0, srcip, srcport, "a"},
			receive "a" -> true after 500 -> throw("Timeout!") end
		end)
	].

inverse_send_rtpc_test(Pid) ->
	[
		%% remote sends a packet... but destin is unknown
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 3, srcip, srcport, "b"},
			receive "b" -> throw("Sended?!?") after 500 -> true end
		end),

		%% local sends a packet and is relayed to remote
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {udp, 1, srcip, srcport, "a"},
			receive "a" -> true after 500 -> throw("Timeout!") end
		end)
	].

redirect_remote_test(Pid) ->
	[
		?_assert(begin
			P = self(),
			meck:expect(gen_udp, send, fun(_Sock, _Addr, _Port, Data) ->
				P ! Data,
				ok
			end),
			Pid ! {redirect_remote, <<"+34666555444@test.com">>, <<"127.0.0.1">>, <<"1234">>},
			receive <<0,1,36:16,_IPort:128,6:16,32:16,_Username/binary>> -> true after 500 -> throw("Timeout!") end
		end)
	].