
-module(wstest_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

get_app_env(Env, Default) ->
    case application:get_env(wstest, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

init([]) ->
    
    Ip = get_app_env(web_ip, "0.0.0.0"),
    Port = get_app_env(web_port, 8000),
    LogDir = get_app_env(log_dir, "priv/log"),

    {ok, Dispatch} = file:consult(filename:join(
		                            code:priv_dir(wstest),
                                    "dispatch.conf")),

    WebConfig = [
                 {ip, Ip},
                 {port, Port},
                 {log_dir, LogDir},
                 {dispatch, Dispatch}
                 ],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [webmachine_mochiweb]},

    {ok, {{one_for_one, 5, 10}, [Web]}}.

