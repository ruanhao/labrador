%%%----------------------------------------------------------------------
%%% File    : dallas_assist_http_pid.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Get proc information or kill the proc.
%%% Created : Apr 9, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_http_pid).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	IsKill = qs_val(<<"kill">>, Req),
	PidBin = qs_val(<<"pid">>, Req), 
	Pid = list_to_pid(binary_to_list(PidBin)), 
	if 
		IsKill =:= undefined -> 
			proc_info(Pid, Req, State);
		true -> 
			proc_kill(Pid, Req, State)
	end.

terminate(_Reason, _Req, _State) ->
	ok.

proc_kill(Pid, Req, State) -> 
	exit(Pid, kill), 
	{ok, Req, State}.

proc_info(Pid, Req, State) -> 
	Node = node(Pid), 
	ProcInfo1 = rpc:call(Node, erlang, process_info, [Pid]), 
	ProcInfo = case ProcInfo1 of 
				   undefined -> [{no_pid, "The Pid Just Run Away :)"}];
				   _ -> ProcInfo1
			   end,
	Body = jsx:term_to_json(ProcInfo),
	Headers = [{<<"Content-Type">>, <<"application/json">>}],
	{ok, Req1} = cowboy_req:reply(200, Headers, Body, Req),
	{ok, Req1, State}.

qs_val(Key, Req) when is_binary(Key) -> 
	{Val, _} = cowboy_req:qs_val(Key, Req), 
	Val.
