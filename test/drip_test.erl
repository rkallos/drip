-module(drip_test).

-include_lib("eunit/include/eunit.hrl").

-include("include/drip.hrl").

drip_empty_or_missing_calls_test_() ->
    {setup, fun setup/0, fun teardown/1, [fun empty_or_missing_calls/0]}.

drip_static_rules_test_() ->
    {setup, fun setup/0, fun teardown/1, [fun static_rules/0]}.

drip_time_based_rules_test_() ->
    {setup, fun setup/0, fun teardown/1, [fun time_based_rules/0]}.

drip_calculate_new_sample_rates_test_() ->
    {setup, fun setup/0, fun teardown/1, [fun calculate_new_sample_rates/0]}.

empty_or_missing_calls() ->
    ?assertEqual(false, drip:sample(foo)),
    ?assertEqual(false, drip:sample({foo, bar})),
    ?assertEqual(true, drip:all([])),
    ?assertEqual(false, drip:any([])),
    ?assertEqual(undefined, drip:rate(foo)),
    ?assertEqual(undefined, drip:rate({foo, missing_key})).

static_rules() ->
    drip:add_rule(foo, drip_rule:static(1)),
    drip:add_rule(bar, drip_rule:static(1, fun(_) -> 1 end)),
    drip:add_rule(baz, drip_rule:static(4294967295)),
    ?assertEqual(1, drip:rate(foo)),
    ?assertEqual(1, drip:rate(bar)),
    ?assertEqual(4294967295, drip:rate(baz)),
    ?assertEqual(true, drip:sample(foo)),
    ?assertEqual(true, drip:sample(bar)),
    ?assertEqual(false, drip:sample(baz)),
    ?assertEqual(false, drip:sample(not_a_key)),
    ?assertEqual(true, drip:any([baz, bar, foo])),
    ?assertEqual(false, drip:all([foo, bar, baz])).

time_based_rules() ->
    drip:add_rule(foo, drip_rule:time_based(1)),
    Options = [{sample_rate, 2}, {rng, fun(_) -> 1 end}, {ms_period, 10000}],
    drip:add_rule(bar, drip_rule:time_based(5, Options)),
    ?assertEqual(true, drip:sample(foo)),
    ?assertEqual(true, drip:sample(bar)),
    ?assertEqual(1, drip:rate(foo)),
    ?assertEqual(2, drip:rate(bar)),

    drip:add_rule(baz, drip_rule:time_based(1)),
    ets:insert(?DRIP_TABLE, {{count, baz}, 4294967296}),
    drip:add_rule(bob, drip_rule:time_based(1)),
    Samples = [{foo, 99}, {bar, 249}],
    [[drip:sample(K) || _ <- lists:seq(1, N)] || {K, N} <- Samples],
    drip_server ! {timeout, blah, {foo, erlang:system_time(second) - 10}},
    drip_server ! {timeout, blah, {bar, erlang:system_time(second) - 10}},
    drip_server ! {timeout, blah, {baz, erlang:system_time(second)}},
    drip_server ! {timeout, blah, {bob, erlang:system_time(second)}},
    timer:sleep(100),
    ?assertEqual(10, drip:rate(foo)),
    ?assertEqual(5, drip:rate(bar)),
    ?assertEqual(4294967295, drip:rate(baz)),
    ?assertEqual(1, drip:rate(bob)).

calculate_new_sample_rates() ->
    Desired = 10,
    Samples = [{a, 100}, {b, 10}, {c, 2}],
    Expected = #{a => 15, b => 3, c => 2},
    Actual = drip_server:calculate_new_sample_rates(Samples, Desired),
    ?assertEqual(Expected, Actual),

    drip:add_rule(foo, drip_rule:avg(Desired)),
    [[drip:sample({foo, K}) || _ <- lists:seq(1, N)] || {K, N} <- Samples],
    drip_server ! {timeout, blah, {foo, 0}},
    timer:sleep(100),
    ?assertEqual(15, drip:rate({foo, a})),
    ?assertEqual(3, drip:rate({foo, b})),
    ?assertEqual(2, drip:rate({foo, c})),

    [[drip:sample({foo, K}) || _ <- lists:seq(1, N)] || {K, N} <- Samples],
    drip_server ! {timeout, blah, {foo, erlang:system_time(second) - 15}},
    ok.

% Bunch of random stuff to boost coverage %
start_stop_and_other_test() ->
    Rules = #{
        foo => drip_rule:static(1),
        bar => drip_rule:static(1),
        baz => drip_rule:static(4294967295)
    },
    ok = application:set_env(drip, rules, Rules),
    {ok, _} = application:ensure_all_started(drip),
    ok = gen_server:call(drip_server, blah),
    ok = gen_server:cast(drip_server, blah),
    drip_server ! {timeout, blah, {not_a_key, 0}},
    drip_server ! not_a_message,
    drip_server:terminate(normal, sys:get_state(drip_server)),
    ok = application:stop(drip).

setup() ->
    error_logger:tty(false),
    {ok, _} = application:ensure_all_started(drip).

teardown(_) ->
    application:stop(drip).
