-module(drip_test).

-include_lib("eunit/include/eunit.hrl").

-include("include/drip.hrl").

drip_test_() ->
    {setup, fun() -> application:ensure_all_started(drip) end, fun(_) -> application:stop(drip) end,
        [
            fun static_rules/0,
            fun calculate_new_sample_rates/0
        ]}.

static_rules() ->
    drip:add_rule(foo, drip_rule:static(1)),
    drip:add_rule(bar, drip_rule:static(1)),
    drip:add_rule(baz, drip_rule:static(4294967295)),
    ?assertEqual(drip:sample(foo), true),
    ?assertEqual(drip:sample(bar), true),
    ?assertEqual(drip:sample(baz), false),
    ?assertEqual(drip:sample(not_a_key), false),
    ?assertEqual(drip:any([foo, bar, baz]), true),
    ?assertEqual(drip:all([foo, bar, baz]), false).

calculate_new_sample_rates() ->
    Desired = 10,
    Samples = [{a, 100}, {b, 10}, {c, 2}],
    Expected = #{a => 15, b => 3, c => 2},
    Actual = drip_server:calculate_new_sample_rates(Samples, Desired),
    ?assertEqual(Actual, Expected),

    drip:add_rule(foo, drip_rule:avg(Desired)),
    [[drip:sample({foo, K}) || _ <- lists:seq(1, N)] || {K, N} <- Samples],
    erlang:display(ets:tab2list(?DRIP_TABLE)),
    drip_server ! {timeout, blah, {foo, 0}},
    timer:sleep(100),
    ?assertEqual(drip:rate({foo, a}), 15),
    ?assertEqual(drip:rate({foo, b}), 3),
    ?assertEqual(drip:rate({foo, c}), 2).
