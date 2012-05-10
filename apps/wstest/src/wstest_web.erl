-module(wstest_web).
-export([start/1, stop/0, handler/1, start_greet/0, process/5]).

mime_types() ->
	[{"text/plain", text}, {"application/xml", xml}, {"text/xml", xml}].

start(Options) ->
	io:format("Options=~p~n", [Options]),
	register(dispatcher, spawn_link(fun dispatcher/0)),
	mochiweb_http:start([{name, wstest_web}, {loop, fun handler/1} | Options]).

stop() ->
	io:format("Stop!~n"),
	mochiweb_http:stop(wstest_web).

start_greet() -> spawn(fun() -> greet("Ciao!\n") end).
greet(Message) ->
	receive
		Newmessage -> greet(Newmessage)
	after 1000 ->
		dispatcher ! {broadcast, Message},
		greet(Message)
	end.

dispatcher() ->
	dispatcher([], 0).

dispatcher(Handlers, Counter) ->
	io:format("Dispatcher invoked~n~n"),
	receive
		{broadcast, Message} -> broadcast(Handlers, io_lib:format("~s~p\n", [Message, Counter])), dispatcher(Handlers, Counter + 1);
		{addhandler, Handler} -> dispatcher([Handler|Handlers], Counter);
		{removehandler, Handler} -> dispatcher(Handlers -- [Handler], Counter);
		_ -> dispatcher(Handlers, Counter)
	end.

broadcast([Handler | Handlers], Message) ->
	try Handler ! {echo, Message} of
		_ -> ok
	catch 
		exit:badarg -> dispatcher ! {removehandler, Handler}
	end,
	broadcast(Handlers, Message);
broadcast([], _) ->
	ok.

send_chunks(Req, Resp, Tail) ->
	io:format("Adding: ~p~n", [self()]),
	dispatcher ! {addhandler, self()},
	send_chunks(Req, Resp, Tail, 100),
	Resp.

send_chunks(_Req, Resp, Tail, 0) ->
	Resp:write_chunk(Tail),	
	Resp:write_chunk(<<>>),
	dispatcher ! {removehandler, self()},
	io:format("Removing: ~p~n", [self()]);
send_chunks(Req, Resp, Tail, Count) ->
	receive
		{echo, Message} -> Resp:write_chunk(Message)
	end,
	send_chunks(Req, Resp, Tail, Count -1).

receive_chunks(Req) ->
	Req:stream_body(1024, fun receive_chunks/2, []).

receive_chunks({_Size, Bin}, State) ->
	dispatcher ! {broadcast, Bin},
	[Bin | State].

process(Req, _, xml, 'GET', "test") ->
	Resp = Req:ok({"application/xml", [], chunked}),
	Resp:write_chunk("<?xml version=\"1.0\"?>\n"),
	Resp:write_chunk("<hello>\n"),
	send_chunks(Req, Resp, "</hello\n>");
process(Req, _, text, 'GET', "test") ->
	Resp = Req:ok({"text/plain", [], chunked}),
	Resp:write_chunk("Hello!\n"),
	send_chunks(Req, Resp, "Bye\n");
process(Req, _, _, 'POST', "test") ->
	receive_chunks(Req),
	Req:ok({"text/plain", "Sent"});
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
	?MODULE:process(Req, Consumes, Produces, Method, Path).