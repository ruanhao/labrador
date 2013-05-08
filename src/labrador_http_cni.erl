%%%----------------------------------------------------------------------
%%% File      : labrador_http_cni.erl
%%% Author    : ryan.ruan@ericsson.com
%%% Purpose   : Get central node information.
%%% Created   : Apr 8, 2013
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

-module(labrador_http_cni).

-behaviour(cowboy_http_handler).

%% Behaviour Callbacks
-export([init/3, handle/2, terminate/3]).

%% ===================================================================
%% Behaviour Callbacks
%% ===================================================================
init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	CNode = labrador_util:get_cnode(),
	LProcs = rpc:call(CNode, erlang, system_info, [logical_processors]), 
	OtpRls = rpc:call(CNode, erlang, system_info, [otp_release]), 
	SysArch = rpc:call(CNode, erlang, system_info, [system_architecture]),
	CNodeInfo = [{cnode, CNode}, {lp, LProcs}, {otpr, OtpRls}, {sa, SysArch}],
	Body = jsx:term_to_json(CNodeInfo),
	Headers = [{<<"Content-Type">>, <<"application/json">>}],
	{ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
