-module(wstest_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MochiOptions = [{ip, "0.0.0.0"}, {port, 8001}, {nodelay, true}],
	
	error_logger:logfile({open, "wstest.log"}),
	error_logger:info_msg("Options=~p~n", [MochiOptions]),
	
    MochiwebChild = {
		mochiweb,
		{wstest_web, start_mochiweb, [MochiOptions]},
		permanent, 5000, worker, [wstest_web]
	},
	DispatcherChild = {
		dispatcher,
		{wstest_web, start_dispatcher, []},
		permanent, 5000, worker, [wstest_web]
	},
	HostChild = {
		host,
		{wstest_web, start_host, []},
		permanent, 5000, worker, [wstest_web]
	},
	FrameChild = {
		frame,
		{wstest_web, start_frame, []},
		permanent, 5000, worker, [wstest_web]
	},
	RepeaterChild = {
		repeater,
		{wstest_web, start_repeater, []},
		permanent, 5000, worker, [wstest_web]
	},
    {ok, {{one_for_one, 5, 10}, [DispatcherChild, HostChild, FrameChild, RepeaterChild, MochiwebChild]}}.
