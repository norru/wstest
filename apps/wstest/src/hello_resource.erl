-module(hello_resource).

-export([init/1,
		charsets_provided/2,
		to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, nostate}.

charsets_provided(RD, Ctx) ->
    {[{"utf-8", fun(C) -> C end}], RD, Ctx}.

to_html(RD, Ctx) ->
    C = "<html><head></head><body>Hello, world!</body></html>",
    {C, RD, Ctx}.
