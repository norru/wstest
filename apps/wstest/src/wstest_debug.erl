-module(wstest_debug).
-export([start/0]).

start() ->
	code:add_pathsa(["../../deps/mochiweb/ebin", "../../apps/wstest/ebin"]),
	lists:foreach(fun(Application) -> application:start(Application) end, [crypto, inets, mochiweb, wstest]),
	reloader:start().