-module(wstest_web).
-export([dispatcher/2, repeater/1, repeater_accept/1, frame/1, host/1,
		 start_dispatcher/0, start_frame/0, start_repeater/0, start_host/0,start_mochiweb/1]).

mime_types() ->[
				{"text/plain", text},
				{"application/xml", xml},
				{"text/xml", xml}, 
				{"application/json", json},
				{"text/html", html}
			   ].

auto_register(Name, Args) ->
	Pid = spawn_link(?MODULE, Name, Args),
	register(Name, Pid),
	{ok, Pid}.
  
start_dispatcher() -> auto_register(dispatcher, [[], 0]).
start_frame() -> auto_register(frame, [{0, [], 200}]).
start_repeater() -> auto_register(repeater, [8002]).
start_host() -> auto_register(host, [undefined]).

start_mochiweb(Options) ->
	mochiweb_http:start([{name, wstest_web}, {loop, fun handler/1} | Options]).

log(info, Format, Data) -> error_logger:info_msg(Format, Data);
log(warn, Format, Data) -> error_logger:warning_msg(Format, Data);
log(_, Format, Data) -> error_logger:error_msg(Format, Data).

repeater(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, line}, {active, false}, {buffer, 10 * 1024}]) of
		{ok, ListenSocket} ->
			repeater_accept(ListenSocket);
		{error, Reason} ->
			log(error, "Socket error", Reason),
			exit(Reason)
	end.
			
repeater_accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	    {ok, Socket} ->
			%% last wins!
			host ! {init, Socket};
	    AcceptResult ->
			log(error, "Socket error", AcceptResult)
    end,
	?MODULE:repeater_accept(ListenSocket).

host(Socket) ->
	receive
		{init, NewSocket} ->
			spawn_link(fun() -> feed(NewSocket) end);
		{chat, Recipient, Text} ->
			send_chat_message(Socket, Recipient, Text),
			NewSocket = Socket;
		{click, Points} ->
			send_chat_message(Socket, "@onclick", points_to_string(Points)),
			NewSocket = Socket;
		Other ->
			log(warn, "Unknown message: ~p", [Other]),
			NewSocket = Socket
	end,
	?MODULE:host(NewSocket).

feed(Socket) ->
	case gen_tcp:recv(Socket, 0) of
	{ok, Packet} ->
		frame ! {set, json2points(Packet)},
		feed(Socket);
	{error, closed} ->
		ok;
	Other ->
		exit(Other)
	end.

send_chat_message(undefined, Recipient, Text) ->
	log(error, "Chat not established, dropping ~s: ~s", [Recipient, Text]);
send_chat_message(Socket, Recipient, Text) ->
	Packet = [Recipient, ": ", Text, "\n"],
	case gen_tcp:send(Socket, Packet) of
		{error, closed} ->
			ok;
		Other ->
			Other
	end.


frame({Counter, Points, Timeout}) ->
	broadcast_points(Points),
	receive
		clear ->
			{NewCounter, NewPoints, NewTimeout} = {Counter, [], Timeout};
		{set, NewPoints} ->
			{NewCounter, NewTimeout} = {Counter, Timeout};
		{add, Point} ->
			{NewCounter, NewPoints, NewTimeout} = {Counter, [Point | Points], Timeout};
		{addmany, ManyPoints} ->
			{NewCounter, NewPoints, NewTimeout} = {Counter, ManyPoints ++ Points, Timeout};
		{timeout, NewTimeout} ->
			{NewCounter, NewPoints} = {Counter, Points};
		refresh ->
			{NewCounter, NewPoints, NewTimeout} = {Counter, Points, Timeout};
		die -> 
			{NewCounter, NewPoints, NewTimeout} = {Counter, Points, Timeout},
			exit(normal)
	end,
	?MODULE:frame({NewCounter, NewPoints, NewTimeout}).

broadcast(Message) ->
	dispatcher ! {broadcast, Message}.

dispatcher(Handlers, Counter) ->
	process_flag(trap_exit, true),
	receive
		{broadcast, Message} ->
			broadcast(Handlers, [Message, "\n"]),
			{NewHandlers, NewCounter} = {Handlers, Counter + 1};
		{addhandler, Handler} ->
			link(Handler),
			{NewHandlers, NewCounter} = {[Handler|Handlers], Counter},
			log(info, "Adding: ~p,~p~n", [Handler, NewHandlers]);
		{removehandler, Handler} ->
			{NewHandlers, NewCounter} = {Handlers -- [Handler], Counter},
			log(info, "Removing: ~p,~p~n", [Handler, NewHandlers]);
		{'EXIT', Handler, Reason} ->
			{NewHandlers, NewCounter} = {Handlers -- [Handler], Counter},
			log(info, "Terminated: ~p,~p,~p~n", [Handler, NewHandlers, Reason]);
		_ ->
			{NewHandlers, NewCounter} = {Handlers, Counter}
	end,
	?MODULE:dispatcher(NewHandlers, NewCounter).

broadcast_points(PtsList) ->
	broadcast(io_lib:format("[~s]", [points_to_string(PtsList)])).

points_to_string([{X, Y} | PtsList]) ->
	io_lib:format("[~p,~p]", [X, Y]) ++ 
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
	frame ! refresh,
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
  
json2points(Body) ->
	[ {X, Y} || [X, Y] <- mochijson2:decode(Body)].
json2chat(Body) ->
	Json = mochijson2:decode(Body),
	[Recipient, Text] = Json,
	{chat, Recipient, Text}. 

process(Req, _, _, 'GET', "feed") ->
	Resp = Req:ok({"text/event-stream", [], chunked}),
	send_chunks(Req, Resp, "[]");
process(Req, _, _, 'POST', "feed") ->
	receive_chunks(Req),
	Req:ok({"text/plain", "Sent"});
process(Req, json, _, 'POST', "add") ->
	frame ! {addmany, json2points(Req:recv_body())},
	host ! {click, json2points(Req:recv_body())},
	Req:ok({"text/plain", "Sent"});
process(Req, json, _, 'POST', "chat") ->
	chat ! json2chat(Req:recv_body()),
	Req:ok({"text/plain", "Sent"});
process(Req, _, _, 'GET', Page) ->
	case file:read_file("../../apps/wstest/src/" ++ Page) of
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
	Consumes = consumes(Req),
	Produces = produces(Req),
	Method = Req:get(method),
	process(Req, Consumes, Produces, Method, Path).