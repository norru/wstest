-module(wstest_web).

-export([start/1, stop/0, handler/1]).

start(Options) ->
	io:format("Options=~p~n", [Options]),

	Status = mochiweb_http:start([
								  {name, ?MODULE},
								  {loop, fun handler/1} |
								  Options
								 ]
								),
	Status.

stop() ->
	io:format("Stop!~n"),
    mochiweb_http:stop(?MODULE).

handler(Req) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        _ ->
			io:format("Path=~p, Req=~p~n", [Path, Req]),
            Req:respond({501, [], []})
    end.