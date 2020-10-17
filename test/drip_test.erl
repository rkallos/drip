-module(drip_test).

-include_lib("eunit/include/eunit.hrl").

drip_test_() ->
    {setup, fun() -> application:ensure_all_started(drip) end, fun(_) -> application:stop(drip) end,
        [
            fun static_rules/0
        ]}.

static_rules() ->
    drip:add_rule(foo, {static, 1}),
    drip:add_rule(bar, {static, 1}),
    drip:add_rule(baz, {static, 4294967295}),
    ?assertEqual(drip:sample(foo), true),
    ?assertEqual(drip:sample(bar), true),
    ?assertEqual(drip:sample(baz), false),
    ?assertEqual(drip:sample(not_a_key), false),
    ?assertEqual(drip:any([foo, bar, baz]), true),
    ?assertEqual(drip:all([foo, bar, baz]), false).
