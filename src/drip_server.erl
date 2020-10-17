-module(drip_server).

-include("drip.hrl").

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

-spec add_rule(drip:key(), drip:sampling_rule()) -> ok.
add_rule(Key, Rule) ->
    gen_server:call(?SERVER, {add_rule, Key, Rule}).

-spec start_link() ->
    {ok, Pid :: pid()}
    | {error, Error :: {already_started, pid()}}
    | {error, Error :: term()}
    | ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init(Args :: term()) ->
    {ok, State :: term()}
    | {ok, State :: term(), Timeout :: timeout()}
    | {ok, State :: term(), hibernate}
    | {stop, Reason :: term()}
    | ignore.
init([]) ->
    Rules =
        case application:get_env(rules) of
            {ok, Rs} -> Rs;
            undefined -> #{}
        end,
    add_rules_to_table(Rules),
    {ok, #state{rules = Rules}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
    {reply, Reply :: term(), NewState :: term()}
    | {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()}
    | {reply, Reply :: term(), NewState :: term(), hibernate}
    | {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: term(), Reply :: term(), NewState :: term()}
    | {stop, Reason :: term(), NewState :: term()}.
handle_call({add_rule, Key, Rule}, _From, State = #state{rules = Rules}) ->
    add_rule_to_table(Key, Rule),
    {reply, ok, State#state{rules = Rules#{Key => Rule}}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()}
    | {noreply, NewState :: term(), Timeout :: timeout()}
    | {noreply, NewState :: term(), hibernate}
    | {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    State :: term()
) -> any().
terminate(_Reason, _State) ->
    true = ets:delete_all_objects(?DRIP_TABLE),
    ok.

% private

-spec add_rules_to_table(drip:ruleset()) -> ok.
add_rules_to_table(Rules) ->
    lists:foreach(fun add_rule_to_table/1, maps:to_list(Rules)).

-spec add_rule_to_table({drip:key(), drip:sampling_rule()}) -> true.
add_rule_to_table({Key, Rule}) ->
    add_rule_to_table(Key, Rule).

-spec add_rule_to_table(drip:key(), drip:sampling_rule()) -> true.
add_rule_to_table(Key, Rule) ->
    ets:insert(?DRIP_TABLE, {{rule, Key}, Rule}).
