-module(wstest_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MochiOptions = [{ip, "0.0.0.0"}, {port, 8001}, {nodelay, true}],
	
    Mochionly = {
		wstest_web,
		{wstest_web, start, [MochiOptions]},
		permanent, 5000, worker, [wstest_web]
	},
	
    {ok, {{one_for_one, 5, 10}, [Mochionly]}}.
