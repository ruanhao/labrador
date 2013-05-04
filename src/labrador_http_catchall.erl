%%%----------------------------------------------------------------------
%%% File    : labrador_http_catchall.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Handle all unexpected URL.
%%% Created : Apr 11, 2013
%%%----------------------------------------------------------------------
-module(labrador_http_catchall).
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

