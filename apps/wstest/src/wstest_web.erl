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

xml_body(_) ->
  "<?xml version=\"1.0\"?><hello/>".

process(Req, xml, 'GET', "test") ->
	Req:respond({200, [{"Content-Type", "text/xml"}], xml_body(Req)});
process(Req, xml, 'POST', _) ->
	Req:respond({501,[],[]});
process(Req, _, _, _)->
	Req:not_found().

handler(Req) ->
	"/" ++ Path = Req:get(path),
	io:format("Path=~p, Req=~p~n", [Path, Req]),
	process(Req, xml, Req:get(method), Path).