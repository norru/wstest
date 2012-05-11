-module(wstest_web).
-export([start/1, stop/0, handler/1, start_greet/0, process/5]).

mime_types() ->
	[{"text/plain", text}, {"application/xml", xml}, {"text/xml", xml}, {"text/html", html}].

start(Options) ->
	error_logger:logfile({open, "wstest.log"}),
	log(info, "Options=~p~n", [Options]),
	register(dispatcher, spawn_link(fun dispatcher/0)),
	mochiweb_http:start([{name, wstest_web}, {loop, fun handler/1} | Options]).

stop() ->
	log(info, "Stop!~n"),
	mochiweb_http:stop(wstest_web).

log(Level, Format) -> log(Level, Format, []).
log(info, Format, Data) -> error_logger:info_msg(Format, Data);
log(warn, Format, Data) -> error_logger:warning_msg(Format, Data);
log(_, Format, Data) -> error_logger:error_msg(Format, Data).

start_greet() -> spawn(fun() -> greet("Ciao!", 200) end).

greet(Message, Timeout) ->
	receive
		Newmessage -> greet(Newmessage, Timeout)
	after Timeout ->
		dispatcher ! {broadcast, Message},
		greet(Message, Timeout)
	end.

dispatcher() ->
	dispatcher([], 0).

dispatcher(Handlers, Counter) ->
	log(info, "Dispatcher invoked~n~n"),
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
	log(info, "Adding: ~p~n", [self()]),
	dispatcher ! {addhandler, self()},
	send_chunks(Req, Resp, Tail, 100),
	Resp.

send_chunks(_Req, Resp, Tail, 0) ->
	send_event(Resp, 'end', Tail),
	Resp:write_chunk(<<>>),
	dispatcher ! {removehandler, self()},
	log(info, "Removing: ~p~n", [self()]);
send_chunks(Req, Resp, Tail, Count) ->
	receive
		{echo, Message} -> send_event(Resp, data, Message)
	end,
	send_chunks(Req, Resp, Tail, Count -1).

receive_chunks(Req) ->
	Req:stream_body(1024, fun receive_chunks/2, []).

receive_chunks({_Size, Bin}, State) ->
	dispatcher ! {broadcast, Bin},
	[Bin | State].

send_event(Resp, Type, Message) ->
	Resp:write_chunk(io_lib:format("~p: ~s~n~n", [Type, Message])).
  
process(Req, _, _, 'GET', "feed") ->
	Resp = Req:ok({"text/event-stream", [], chunked}),
	send_event(Resp, data, "Hello!"),
	send_chunks(Req, Resp, "Bye\n");
process(Req, _, _, 'POST', "test") ->
	receive_chunks(Req),
	Req:ok({"text/plain", "Sent"});
process(Req, _, _, 'GET', Page) ->
	case file:read_file(Page) of
		{ok,FileBin} ->
			Req:ok({"text/html", [{"Content-Disposition", "inline"}], FileBin});
		_ ->
			log(error, "Not found: ~p", [Page]),
			Req:not_found()
	end;
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
	log(info, "Path=~p, Req=~p~n", [Path, Req]),
	Consumes = consumes(Req),
	Produces = produces(Req),
	Method = Req:get(method),
	?MODULE:process(Req, Consumes, Produces, Method, Path).