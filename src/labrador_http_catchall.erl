%%%----------------------------------------------------------------------
%%% File      : labrador_http_catchall.erl
%%% Author    : ryan.ruan@ericsson.com
%%% Purpose   : Catch all HTTP requests that are not expected.
%%% Created   : Apr 11, 2013
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

-module(labrador_http_catchall).

-behaviour(cowboy_http_handler).

-define(PAGENOTFOUND, "html/404.html").

%% Behaviour Callbacks
-export([init/3, handle/2, terminate/3]).

%% ===================================================================
%% Behaviour Callbacks
%% ===================================================================
init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    case labrador:file(?PAGENOTFOUND) of 
        {ok, Body} -> 
            {ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Body, Req),
            {ok, Req2, State};
        _ ->    %% No 404 Not Found Page 
            {ok, Req2} = cowboy_req:reply(404, [], <<"<h1>404</h1>">>, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
