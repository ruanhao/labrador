%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
-module(labrador_http_catchall).

-created('Date: 2013/04/11').
-author('ryan.ruan@ericsson.com').
-revision('Revision: 2.1').

-behaviour(cowboy_http_handler).
-define(PAGENOTFOUND, "html/404.html").
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
	labrador:msg_trace(?LINE, process_info(self(), current_function), "init", []),
    {ok, Req, undefined_state}.

handle(Req, State) ->
	labrador:msg_trace(?LINE, process_info(self(), current_function), "handle catch all, Req: ~p", [Req]),
	case labrador:file(?PAGENOTFOUND) of 
				{ok, Body} -> 
	                labrador:msg_trace(?LINE, process_info(self(), current_function), "here1", []),
					{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Body, Req),
					{ok, Req2, State};
				_ ->	%% No 404 Not Found Page 
	                labrador:msg_trace(?LINE, process_info(self(), current_function), "here2", []),
					{ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
					{ok, Req2, State}
			end.

terminate(_Reason, _Req, _State) ->
    ok.

