%%%----------------------------------------------------------------------
%%% File      : labrador_http_static.erl
%%% Author    : SMELLS LIKE BEAM SPIRIT
%%% Modifier  : ryan.ruan@ericsson.com
%%% Purpose   : Handle all static HTML requests.
%%% Created   : Apr 3, 2013
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved online at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%----------------------------------------------------------------------

-module(labrador_http_static).

-define(PAGENOTFOUND, "html/404.html").

-behaviour(cowboy_http_handler).

%% Behaviour Callbacks
-export([init/3, handle/2, terminate/3]).

%% ===================================================================
%% Behaviour Callbacks
%% ===================================================================
init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, [File]) ->
  % File is like: <<"html/index.html">>	
  {ok, Req, File}.

handle(Req, undefined_state = State) ->
  {Path, Req2} = cowboy_req:path(Req), % Path is like: <<"/js/main.js">>
  send(Req2, Path, State);

handle(Req, File = State) ->
  send(Req, File, State).

%% ===================================================================
%% Inner Functions
%% ===================================================================
send(Req, PathBin, State) ->
	Path = reform_path(PathBin),
	case labrador_util:file(Path) of
		{ok, Body} ->
			Headers    = [content_type_header(Path)],
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
