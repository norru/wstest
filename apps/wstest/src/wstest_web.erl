-module(wstest_web).
-export([start/1, stop/0]).

mime_types() ->[
				{"text/plain", text},
				{"application/xml", xml},
				{"text/xml", xml}, 
				{"application/json", json},
				{"text/html", html}
			   ].

start(Options) ->
	error_logger:logfile({open, "wstest.log"}),
	log(info, "Options=~p~n", [Options]),
	register(dispatcher, spawn_link(fun dispatcher/0)),
	register(anim, spawn(fun() -> frame({0, [{256,256} || _ <- lists:seq(1,100)], 200}) end)),
	mochiweb_http:start([{name, wstest_web}, {loop, fun handler/1} | Options]).

stop() ->
	log(info, "Stop!~n"),
	mochiweb_http:stop(wstest_web).

log(Level, Format) -> log(Level, Format, []).
log(info, Format, Data) -> error_logger:info_msg(Format, Data);
log(warn, Format, Data) -> error_logger:warning_msg(Format, Data);
log(_, Format, Data) -> error_logger:error_msg(Format, Data).

point_transform(X, Y) ->
	{X + random:uniform(5) - 3, Y + random:uniform(5) - 3}.

frame({Counter, Points, Timeout}) ->
	broadcast_points(Points),
	receive
		clear ->
			{NewCounter, NewPoints, NewTimeout} = {Counter, [], Timeout};
		{add, Point} ->
			{NewCounter, NewPoints, NewTimeout} = {Counter, [Point | Points], Timeout};
		{addmany, ManyPoints} ->
			{NewCounter, NewPoints, NewTimeout} = {Counter, ManyPoints ++ Points, Timeout};
		{timeout, NewTimeout} ->
			{NewCounter, NewPoints} = {Counter, Points};
		die -> 
			{NewCounter, NewPoints, NewTimeout} = {Counter, Points, Timeout},
			exit(normal)
	after Timeout ->
		{NewCounter, NewTimeout} = {Counter + 1, Timeout},
		NewPoints = [point_transform(X, Y) || {X, Y} <- Points, X >=0, Y >=0, X < 512, Y < 512 ]
	end,
	frame({NewCounter, NewPoints, NewTimeout}).

dispatcher() ->
	dispatcher([], 0).

broadcast(Message) ->
	dispatcher ! {broadcast, Message}.

dispatcher(Handlers, Counter) ->
	process_flag(trap_exit, true),
	receive
		{broadcast, Message} ->
			broadcast(Handlers, io_lib:format("~s\n", [Message])),
			{NewHandlers, NewCounter} = {Handlers, Counter + 1};
		{addhandler, Handler} ->
			link(Handler),
			{NewHandlers, NewCounter} = {[Handler|Handlers], Counter},
			log(info, "Adding: ~p,~p~n", [Handler, NewHandlers]);
		{removehandler, Handler} ->
			{NewHandlers, NewCounter} = {Handlers -- [Handler], Counter},
			log(info, "Removing: ~p,~p~n", [Handler, NewHandlers]);
		{'EXIT',Handler,Reason} ->
			{NewHandlers, NewCounter} = {Handlers -- [Handler], Counter},
			log(info, "Terminated: ~p,~p,~p~n", [Handler, NewHandlers, Reason]);
		_ ->
			{NewHandlers, NewCounter} = {Handlers, Counter}
	end,
	dispatcher(NewHandlers, NewCounter).

broadcast_points(PtsList) ->
	broadcast(io_lib:format("[~s]", [points_to_string(PtsList)])).

points_to_string([{X, Y} | PtsList]) ->
	io_lib:format("{\"x\": ~p, \"y\": ~p}", [X, Y]) ++ 
	case PtsList of
		[] -> "";
		_ -> "," ++ points_to_string(PtsList)
	end;
points_to_string([]) ->
	[].

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
	dispatcher ! {addhandler, self()},
	send_chunks(Req, Resp, Tail, 65535),
	Resp.

send_chunks(_, Resp, Tail, 0) ->
	send_event(Resp, 'end', Tail),
	Resp:write_chunk(<<>>),
	dispatcher ! {removehandler, self()},
	ok;
send_chunks(Req, Resp, Tail, Count) ->
	receive
		{echo, Message} -> send_event(Resp, data, Message)
	end,
	send_chunks(Req, Resp, Tail, Count - 1).

receive_chunks(Req) ->
	Req:stream_body(1024, fun receive_chunks/2, []).
receive_chunks({0, Bin}, State) ->
	[Bin | State];
receive_chunks({_Size, Bin}, State) ->
	dispatcher ! {broadcast, Bin},
	[Bin | State].

send_event(Resp, Type, Message) ->
	Resp:write_chunk(io_lib:format("~p: ~s~n~n", [Type, Message])).
  
process(Req, _, _, 'GET', "feed") ->
	Resp = Req:ok({"text/event-stream", [], chunked}),
	send_event(Resp, data, "[]"),
	send_chunks(Req, Resp, "[]");
process(Req, _, _, 'POST', "feed") ->
	receive_chunks(Req),
	Req:ok({"text/plain", "Sent"});
process(Req, json, _, 'POST', "add") ->
	Points = mochijson2:decode(Req:recv_body()),
	anim ! {addmany, [ {X, Y} || {struct, [{_, X}, {_, Y}]} <- Points] },
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
	log(info, "Path=~p~n", [Path]),
	Consumes = consumes(Req),
	Produces = produces(Req),
	Method = Req:get(method),
	process(Req, Consumes, Produces, Method, Path).