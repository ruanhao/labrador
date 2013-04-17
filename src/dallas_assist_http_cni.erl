%%%----------------------------------------------------------------------
%%% File    : dallas_assist_http_cni.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Get central node information.
%%% Created : Apr 8, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_http_cni).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	CNode = dallas_assist_util:get_cnode(),
	LProcs = rpc:call(CNode, erlang, system_info, [logical_processors]), 
	OtpRls = rpc:call(CNode, erlang, system_info, [otp_release]), 
	SysArch = rpc:call(CNode, erlang, system_info, [system_architecture]),
	CNodeInfo = [{cnode, CNode}, {lp, LProcs}, {otpr, OtpRls}, {sa, SysArch}],
	Body = jsx:term_to_json(CNodeInfo),
	Headers = [{<<"Content-Type">>, <<"application/json">>}],
	{ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.