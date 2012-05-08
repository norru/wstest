-module(wstest_web).
-export([start/1, stop/0, handler/1]).

mime_types() ->
	[{"application/xml", xml}, {"text/xml", xml}].

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

process(Req, _, xml, 'GET', "test") ->
	Req:ok({"application/xml", xml_body(Req)});
process(Req, "application/xml", xml, 'POST', _) ->
	Req:respond({201,[],[]});
process(Req, _, _, _, _)->
	Req:not_found().

produces(Req) ->
	produces(Req, mime_types()).
produces(_,[]) ->
	[];
produces(Req,[{MimeType, ContentType} | T]) ->
	case Req:accepts_content_type(MimeType) of
		true -> ContentType;
		_ -> produces(Req, T)
	end.

consumes(Req) ->
	consumes(Req:get_primary_header_value("content-type"), mime_types()).
consumes(_, []) ->
	[];
consumes(GotMimeType, [{MimeType, ContentType} | T]) ->
	case GotMimeType of
		MimeType -> ContentType;
		_ -> consumes(GotMimeType, T)
	end.

handler(Req) ->
	"/" ++ Path = Req:get(path),
	io:format("Path=~p, Req=~p~n", [Path, Req]),
	Consumes = consumes(Req),
	Produces = produces(Req),
	Method = Req:get(method),
	process(Req, Consumes, Produces, Method, Path).