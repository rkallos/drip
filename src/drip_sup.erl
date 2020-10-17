-module(drip_sup).

-include("drip.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    _ = ets:new(?DRIP_TABLE, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => drip_server,
            start => {drip_server, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
