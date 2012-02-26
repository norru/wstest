-module(wstest_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Webmachine = {
		webmachine_mochiweb,
		{webmachine_mochiweb, start, [load_webmachine_config()]},
		permanent, 5000, worker, [webmachine_mochiweb]
	},

	MochiOptions = [{ip, "0.0.0.0"}, {port, 8001}],
	
    Mochionly = {
		wstest_web,
		{wstest_web, start, [MochiOptions]},
		permanent, 5000, worker, [wstest_web]
	},
	
    {ok, {{one_for_one, 5, 10}, [Mochionly, Webmachine]}}.

load_webmachine_config() ->
    
    Ip = get_app_env(web_ip, "0.0.0.0"),
    Port = get_app_env(webmachine_port, 8000),
    LogDir = get_app_env(log_dir, "priv/log"),

    {ok, Dispatch} = file:consult(filename:join(code:priv_dir(wstest), "dispatch.conf")),

    [
		{ip, Ip},
		{port, Port},
		{log_dir, LogDir},
		{dispatch, Dispatch}
	].

get_app_env(Env, Default) ->
	case application:get_env(wstest, Env) of
		{ok, Val} -> Val;
		undefined -> Default
	end.