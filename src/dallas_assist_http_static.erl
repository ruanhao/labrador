%%%----------------------------------------------------------------------
%%% File    : dallas_assist_http_static.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com> 
%%%           (Acknowlegement to BigWig)
%%% Purpose : Handle all static HTML request. 
%%% Created : Apr 3, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_http_static).
-behaviour(cowboy_http_handler).
-define(PAGENOTFOUND, "html/404.html").
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, OnlyFile) ->
  {ok, Req, OnlyFile}.

handle(Req, undefined_state = State) ->
  {[_|Path], Req2} = cowboy_http_req:path(Req), % strip <<"static">>
  send(Req2, Path, State);

handle(Req, OnlyFile = State) ->
  send(Req, OnlyFile, State).

send(Req, PathBins, State) ->
	Path = [binary_to_list(P) || P <- PathBins],
	FullPath = filename:join(Path),
	case dallas_assist_util:file(FullPath) of
		{ok, Body} ->
			Headers = [content_type_header(FullPath)],
			{ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
			{ok, Req2, State};
		_ -> %% 404 Not Found
			case dallas_assist_util:file(?PAGENOTFOUND) of 
				{ok, Body} -> 
					{ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Body, Req),
					{ok, Req2, State};
				_ ->	%% No 404 Not Found Page 
					{ok, Req2} = cowboy_http_req:reply(404, [], <<"<h1>404</h1>">>, Req),
					{ok, Req2, State}
			end
	end.

content_type_header(FullPath) -> 
	Ext = filename:extension(FullPath), 
	case Ext of 
		".css" -> {<<"Content-Type">>, <<"text/css">>};
		".js"  -> {<<"Content-Type">>, <<"text/javascript">>};
		_ 	   -> {<<"Content-Type">>, <<"text/html">>}
	end.

terminate(_Req, _State) ->
  ok.
