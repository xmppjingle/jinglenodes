-module(unit_test).

%%% External exports
-compile(export_all).

%%% Macros
all() ->
    [public_api].


init_per_suite(Conf) ->
    Conf.

end_per_suite(_Conf) ->
    ok.

init_per_testcase(Case, Conf) ->
    io:format("Test case ~p started", [Case]),
    Conf.

end_per_testcase(Case, Conf) ->
    io:format("Test case ~p finished", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% Test Cases
%%%-----------------------------------------------------------------------------
public_api() ->
    [{userdata, [{doc, "Tests the public API."}]}].

public_api(_Conf) ->
    Cover = jn_component:cover_test().

