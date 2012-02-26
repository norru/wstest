-module(wstest_web).
-export([start/1, stop/0, handler/1]).

start(Options) ->
	io:format("Options=~p~n", [Options]),
	mochiweb_http:start([
						 {name, wstest_web},
						 {loop, fun handler/1} |
							 Options
						]
					   ).

stop() ->
	io:format("Stop!~n"),
	mochiweb_http:stop(wstest_web).

handler(Req) ->
	"/" ++ Path = Req:get(path),
	io:format("Path=~p, Req=~p~n", [Path, Req]),
	case Req:get(method) of
		'POST'->
			Req:respond({501,[],[]});
		'GET' ->
			Req:respond({200, [{"Content-Type", "text/html"}], ["<body>Hello, world! " ++ Path ++ "</body>"]});
		_ ->
			Req:not_found()
	end.