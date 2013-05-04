%%%----------------------------------------------------------------------
%%% File    : labrador_http_static.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com> 
%%%           (Acknowlegement to BigWig)
%%% Purpose : Handle all static HTML request. 
%%% Created : Apr 3, 2013
%%%----------------------------------------------------------------------
-module(labrador_http_static).
-behaviour(cowboy_http_handler).
-define(PAGENOTFOUND, "html/404.html").
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, []) ->
  labrador:msg_trace(?LINE, process_info(self(), current_function), "init", []),
  {ok, Req, undefined_state};
init({tcp, http}, Req, [OnlyFile]) ->
  % OnlyFile is like: <<"html/index.html">>	
  labrador:msg_trace(?LINE, process_info(self(), current_function), "OnlyFile: ~p", [OnlyFile]),
  {ok, Req, OnlyFile}.

handle(Req, undefined_state = State) ->
  labrador:msg_trace(?LINE, process_info(self(), current_function), "handle", []),
  labrador:msg_trace(?LINE, process_info(self(), current_function), "path: ~p", [cowboy_req:path(Req)]),
  {Path, Req2} = cowboy_req:path(Req), % Path is like: <<"/js/main.js">>
  send(Req2, Path, State);

handle(Req, OnlyFile = State) ->
  labrador:msg_trace(?LINE, process_info(self(), current_function), "handle", []),
  send(Req, OnlyFile, State).

send(Req, PathBin, State) ->
	Path = reform_path(PathBin),
	labrador:msg_trace(?LINE, process_info(self(), current_function), "reformed Path: ~p", [Path]),
	case labrador_util:file(Path) of
		{ok, Body} ->
			Headers = [content_type_header(Path)],
			{ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
			{ok, Req2, State};
		_ -> %% 404 Not Found
			case labrador_util:file(?PAGENOTFOUND) of 
				{ok, Body} -> 
					{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Body, Req),
					{ok, Req2, State};
				_ ->	%% No 404 Not Found Page 
					{ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
					{ok, Req2, State}
			end
	end.

reform_path(PathBin) -> 
	Path = binary_to_list(PathBin),
	Static = "/static",
	Idx = string:str(Path, Static),
	P = 
		case Idx of 
			0 -> Path;
			1 -> string:sub_string(Path, 1 + length(Static))
		end,
	trim_slash(P).

trim_slash([$/ | T]) -> 
	T;
trim_slash(Path) -> 
	Path.

content_type_header(FullPath) -> 
	Ext = filename:extension(FullPath), 
	case Ext of 
		".css" -> {<<"Content-Type">>, <<"text/css">>};
		".js"  -> {<<"Content-Type">>, <<"text/javascript">>};
		_ 	   -> {<<"Content-Type">>, <<"text/html">>}
	end.

terminate(_Reason, _Req, _State) ->
  ok.
