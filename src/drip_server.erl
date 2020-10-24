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
            Now = erlang:system_time(second),
            NumSamples =
                case ets:take(?DRIP_TABLE, {count, Key}) of
                    [] -> 0;
                    [{_, N}] -> N
                end,
            Rule2 = update_sample_rate(Rule, Now - PrevTime, NumSamples),
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
maybe_start_timer(_Key, _Rule) ->
    ok.

-spec update_sample_rate(drip:rule(), integer(), pos_integer()) -> drip:rule().
update_sample_rate(
    Rule = #time_based{
        desired_per_second = DesiredPerSecond
    },
    PeriodSecs0,
    NumSamples
) ->
    PeriodSecs = lists:max([PeriodSecs0, 1]),
    SamplesPerSecond = NumSamples / PeriodSecs,
    NewSampleRate = round(SamplesPerSecond / DesiredPerSecond),
    Rule#time_based{
        sample_rate = check_rate(NewSampleRate)
    }.

% makes gradualizer happy
-spec check_rate(integer()) -> drip:rand_range().
check_rate(Rate) when Rate >= 1 andalso Rate =< 4294967295 ->
    Rate;
check_rate(Rate) when Rate > 4294967295 ->
    4294967295;
check_rate(Rate) when Rate < 1 ->
    1.
