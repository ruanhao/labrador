%%%----------------------------------------------------------------------
%%% File    : dallas_assist_http_catchall.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Handle all unexpected URL.
%%% Created : Apr 11, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_http_catchall).
-behaviour(cowboy_http_handler).
-define(PAGENOTFOUND, "html/404.html").
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
	case dallas_assist_util:file(?PAGENOTFOUND) of 
				{ok, Body} -> 
					{ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Body, Req),
					{ok, Req2, State};
				_ ->	%% No 404 Not Found Page 
					{ok, Req2} = cowboy_http_req:reply(404, [], <<"<h1>404</h1>">>, Req),
					{ok, Req2, State}
			end.

terminate(_Req, _State) ->
    ok.

