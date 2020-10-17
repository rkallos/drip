-module(drip).

-include("drip.hrl").

-export([
    add_rule/2,
    all/1,
    any/1,
    sample/1
]).

-type key() :: atom().
-type sampling_rule() ::
    {static, 1..4294967295}.

-type ruleset() :: #{key() => sampling_rule()}.

-export_type([
    key/0,
    sampling_rule/0,
    ruleset/0
]).

-spec add_rule(key(), sampling_rule()) -> ok.
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

-spec sample(key()) -> boolean().
sample(Key) ->
    case ets:lookup(?DRIP_TABLE, {rule, Key}) of
        [{_, Rule}] ->
            apply_rule(Key, Rule);
        [] ->
            false
    end.

% private

-spec apply_rule(key(), sampling_rule()) -> boolean().
apply_rule(_Key, {static, Rate}) when Rate >= 1 andalso Rate =< 4294967295 ->
    case granderl:uniform(Rate) of
        1 -> true;
        _ -> false
    end.
