-module(jn_component_test).

-compile(export_all).

% required for eunit to work
-include_lib("eunit/include/eunit.hrl").

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").
-include("../include/jn_component.hrl").

setup_test_() ->
    {setup, 
        fun init_per_suite/0,
        fun end_per_suite/1,
        fun (Config) -> [
            disco_info_test(Config)
        ] end
    }.

init_per_suite() ->
    meck:new(lager),
    meck:expect(lager, info, fun(X,Y) -> error_logger:info(X,Y) end),
    meck:expect(lager, info, fun(X) -> error_logger:info(X) end),
    meck:expect(lager, debug, fun(X, Y) -> error_logger:info(X,Y) end),
    meck:expect(lager, debug, fun(Y) -> error_logger:info(Y) end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Metadata, _Format, _Args, _Size) ->
        ok
    end),
    meck:expect(lager, dispatch_log, fun(_Severity, _Module, _Function, _Line, _Pid, _Traces, _Format, _Args, _TruncSize) ->
        ok
    end),

    application:start(sasl),
    application:start(exmpp),
    jn_component:start_link(#jnstate{
        handler=jingle_handler
    }),
    ok.

end_per_suite(_Config) ->
    register(jn_schedule, spawn_link(fun() ->
        receive {_, From, stop} ->
            gen_server:reply(From, ok), 
            ok 
        end
    end)),
    register(jn_portmonitor, spawn_link(fun() ->
        receive {_, From, stop} ->
            gen_server:reply(From, ok), 
            ok 
        end
    end)),
    gen_server:call(jn_component, stop),
    application:stop(exmpp),
    application:stop(sasl),
    meck:unload(lager),
    ok.

disco_info_test(_Config) ->
    ?_assert(begin
        meck:new(ecomponent),
        meck:expect(ecomponent, send, fun(Data, _Module) ->
            ok
        end),
        meck:new(jn_portmonitor),
        meck:expect(jn_portmonitor, get_port, fun() ->
            {ok, 0}
        end), 
        meck:new(jingle_relay),
        meck:expect(jingle_relay, start, fun(Port, PortB) ->
            {ok, self()}
        end), 
        register(jn_schedule, spawn_link(fun() ->
            receive
                {_GenCast, _Data} -> ok
            end
        end)),
        jn_component ! {iq, #params{ns=?NS_DISCO_INFO, iq=exmpp_xml:element(undefined, 'iq', [
            exmpp_xml:attribute(<<"to">>, <<"relay.localhost">>),
            exmpp_xml:attribute(<<"id">>, <<"jinglenodes-1">>),
            exmpp_xml:attribute(<<"type">>, <<"get">>)
        ], [
            exmpp_xml:element(?NS_DISCO_INFO, 'query', [], []) 
        ])}},
        meck:unload(jn_portmonitor), 
        meck:unload(ecomponent),
        unregister(jn_schedule), 
        true
    end).
 