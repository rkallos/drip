-module(drip_server).

-include("drip.hrl").
-include("drip_internal.hrl").

-export([
    add_rule/2,
    start_link/0
]).

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-ifdef(TEST).
-export([
    calculate_new_sample_rates/2
]).
-endif.

-define(SERVER, ?MODULE).

-record(state, {
    rules = #{} :: drip:ruleset()
}).

-spec add_rule(drip:key(), drip:rule()) -> ok.
add_rule(Key, Rule) ->
    gen_server:call(?SERVER, {add_rule, Key, Rule}).

-spec start_link() ->
    {ok, Pid :: pid()}
    | {error, Error :: {already_started, pid()}}
    | {error, Error :: term()}
    | ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init(Args :: term()) -> {ok, #state{}}.
init([]) ->
    Rules =
        case application:get_env(rules) of
            {ok, Rs} -> Rs;
            undefined -> #{}
        end,
    add_rules_to_table(Rules),
    {ok, #state{rules = Rules}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
    {reply, ok, #state{}}.
handle_call({add_rule, Key, Rule}, _From, State = #state{rules = Rules}) ->
    add_rule_to_table(Key, Rule),
    maybe_start_timer(Key, Rule),
    {reply, ok, State#state{rules = Rules#{Key => Rule}}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) -> {noreply, #state{}}.
handle_info({timeout, _TRef, {Key, PrevTime}}, State = #state{rules = Rules}) ->
    logger:warning("Period timeout for ~p.", [Key]),
    case Rules of
        #{Key := Rule} ->
            Rule2 = update_sample_rate(Key, Rule, PrevTime),
            add_rule_to_table(Key, Rule2),
            maybe_start_timer(Key, Rule2)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: term()
) -> ok.
terminate(_Reason, _State) ->
    true = ets:delete_all_objects(?DRIP_TABLE),
    ok.

% private

-spec add_rules_to_table(drip:ruleset()) -> ok.
add_rules_to_table(Rules) ->
    lists:foreach(fun add_rule_to_table/1, maps:to_list(Rules)).

-spec add_rule_to_table({drip:key(), drip:rule()}) -> true.
add_rule_to_table({Key, Rule}) ->
    add_rule_to_table(Key, Rule).

-spec add_rule_to_table(drip:key(), drip:rule()) -> true.
add_rule_to_table(Key, Rule) ->
    ets:insert(?DRIP_TABLE, {{rule, Key}, Rule}).

-spec maybe_start_timer(drip:key(), drip:rule()) -> ok.
maybe_start_timer(Key, #time_based{
    ms_period = Period
}) ->
    _ = erlang:start_timer(Period, self(), {Key, erlang:system_time(second)}),
    ok;
maybe_start_timer(Key, #average{
    ms_period = Period
}) ->
    _ = erlang:start_timer(Period, self(), {Key, erlang:system_time(second)}),
    ok;
maybe_start_timer(_Key, _Rule) ->
    ok.

-spec update_sample_rate(drip:key(), drip:rule(), integer()) -> drip:rule().
update_sample_rate(
    Key,
    Rule = #time_based{
        desired_per_second = DesiredPerSecond
    },
    PrevTime
) ->
    Now = erlang:system_time(second),
    NumSamples =
        case ets:take(?DRIP_TABLE, {count, Key}) of
            [] -> 0;
            [{_, N}] -> N
        end,
    PeriodSecs = erlang:max(Now - PrevTime, 1),
    SamplesPerSecond = NumSamples / PeriodSecs,
    NewSampleRate = round(SamplesPerSecond / DesiredPerSecond),
    Rule#time_based{
        sample_rate = check_rate(NewSampleRate)
    };
update_sample_rate(
    Key,
    Rule = #average{
        desired_per_second = Desired
    },
    _PrevTime
) ->
    Samples = collect_keyed_samples(Key),
    NewSampleRates = calculate_new_sample_rates(Samples, Desired),
    Rule#average{sample_rates = NewSampleRates}.

% makes gradualizer happy
-spec check_rate(integer()) -> drip:rand_range().
check_rate(Rate) when Rate >= 1 andalso Rate =< 4294967295 ->
    Rate;
check_rate(Rate) when Rate > 4294967295 ->
    4294967295;
check_rate(Rate) when Rate < 1 ->
    1.

-spec collect_keyed_samples(drip:key()) -> [{atom(), non_neg_integer()}].
collect_keyed_samples(RuleKey) ->
    MatchSpec = [{{{count, {RuleKey, '$2'}}, '_'}, [], [{{RuleKey, '$2'}}]}],
    Keys = lists:sort(ets:select(?DRIP_TABLE, MatchSpec)),
    Taken = [ets:take(?DRIP_TABLE, {count, Key}) || Key <- Keys],
    [{InnerKey, NumSamples} || [{{count, {_, InnerKey}}, NumSamples}] <- Taken].

-spec calculate_new_sample_rates([{atom(), non_neg_integer()}], pos_integer()) ->
    #{atom() => drip:rand_range()}.
calculate_new_sample_rates(Samples, Desired) ->
    {Sum, LogSum} = lists:foldl(
        fun({_, N}, {S, L}) ->
            {S + N, L + math:log10(N)}
        end,
        {0, 0},
        Samples
    ),
    % number of times goal was met
    GoalCount = Sum / Desired,
    GoalRatio = GoalCount / LogSum,
    {Res, _, _} = lists:foldl(
        fun({Key, N}, {Map, Extra, KeysRemaining}) ->
            ExtraForKey = Extra / KeysRemaining,
            GoalForKey = erlang:max(1, math:log10(N) * GoalRatio) + ExtraForKey,
            Extra2 = Extra - ExtraForKey,
            {Rate, Extra3} =
                case N of
                    X when X < GoalForKey ->
                        {1, Extra2 + GoalForKey - N};
                    _ ->
                        R =
                            try
                                trunc(math:ceil(N / GoalForKey))
                            catch
                                _ -> 1
                            end,
                        E = Extra2 + GoalForKey - (N / R),
                        {R, E}
                end,
            {Map#{Key => Rate}, Extra3, KeysRemaining - 1}
        end,
        {#{}, 0, length(Samples)},
        Samples
    ),
    Res.
