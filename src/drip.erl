-module(drip).

-include("drip.hrl").
-include("drip_internal.hrl").

-export([
    add_rule/2,
    all/1,
    any/1,
    rate/1,
    sample/1
]).

-type rand_range() :: 1..4294967295.
-type rand_fun() :: fun((drip:rand_range()) -> drip:rand_range()).

-type key() :: atom() | {atom(), term()}.
-type rule() :: #static{} | #time_based{} | #average{}.

-type ruleset() :: #{key() => rule()}.

-export_type([
    key/0,
    rule/0,
    rand_fun/0,
    rand_range/0,
    ruleset/0
]).

-spec add_rule(key(), rule()) -> ok.
add_rule(Key, Rule) ->
    drip_server:add_rule(Key, Rule).

-spec all([key()]) -> boolean().
all([]) ->
    true;
all([Key | Rest]) ->
    case sample(Key) of
        false -> false;
        true -> all(Rest)
    end.

-spec any([key()]) -> boolean().
any([]) ->
    false;
any([Key | Rest]) ->
    case sample(Key) of
        true -> true;
        false -> any(Rest)
    end.

-spec rate(key()) -> rand_range() | undefined.
rate({RuleKey, InnerKey}) ->
    case ets:lookup(?DRIP_TABLE, {rule, RuleKey}) of
        [{_, #average{sample_rates = #{InnerKey := R}}}] -> R;
        _ -> undefined
    end;
rate(Key) ->
    case ets:lookup(?DRIP_TABLE, {rule, Key}) of
        [{_, #time_based{sample_rate = R}}] -> R;
        [{_, #static{rate = R}}] -> R;
        _ -> undefined
    end.

-spec sample(key()) -> boolean().
sample({RuleKey, InnerKey}) ->
    case ets:lookup(?DRIP_TABLE, {rule, RuleKey}) of
        [{_, Rule}] ->
            apply_rule({RuleKey, InnerKey}, Rule);
        [] ->
            false
    end;
sample(Key) ->
    case ets:lookup(?DRIP_TABLE, {rule, Key}) of
        [{_, Rule}] ->
            apply_rule(Key, Rule);
        [] ->
            false
    end.

% private

-spec apply_rule(key(), rule()) -> boolean().
apply_rule(_Key, #static{rate = Rate, rng = Rng}) ->
    rng(Rng, Rate);
apply_rule(Key, #time_based{
    sample_rate = Rate,
    rng = Rng
}) ->
    EtsKey = {count, Key},
    _Count = ets:update_counter(?DRIP_TABLE, EtsKey, {2, 1}, {EtsKey, 0}),
    rng(Rng, Rate);
apply_rule(Key = {_RuleKey, InnerKey}, #average{
    sample_rates = Rates,
    rng = Rng
}) ->
    EtsKey = {count, Key},
    _Count = ets:update_counter(?DRIP_TABLE, EtsKey, {2, 1}, {EtsKey, 0}),
    Rate =
        case Rates of
            #{InnerKey := R} -> R;
            _ -> 1
        end,
    rng(Rng, Rate).

-spec rng(undefined | rand_fun(), rand_range()) -> boolean().
rng(undefined, Rate) ->
    granderl:uniform(Rate) =:= 1;
rng(F, Rate) when is_function(F, 1) ->
    F(Rate) =:= 1.
