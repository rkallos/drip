-module(drip_rule).

-include("drip_internal.hrl").

-export([
    avg/1,
    avg/2,
    static/1,
    time_based/1,
    time_based/2
]).

-ifdef(TEST).
-export([
    static/2
]).
-endif.

static(Rate) ->
    #static{rate = Rate}.

static(Rate, Rng) ->
    #static{rate = Rate, rng = Rng}.

time_based(Desired) ->
    time_based(Desired, []).

time_based(Desired, Options) ->
    #time_based{
        desired_per_second = Desired,
        sample_rate = proplists:get_value(sample_rate, Options, 1),
        rng = proplists:get_value(rng, Options, undefined),
        ms_period = proplists:get_value(ms_period, Options, 15000)
    }.

avg(Desired) ->
    avg(Desired, []).

avg(Desired, Options) ->
    #average{
        desired_per_second = Desired,
        sample_rates = proplists:get_value(sample_rates, Options, #{}),
        rng = proplists:get_value(rng, Options, undefined),
        ms_period = proplists:get_value(ms_period, Options, 15000)
    }.
