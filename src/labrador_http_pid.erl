%%%----------------------------------------------------------------------
%%% File      : labrador_http_pid.erl
%%% Author    : ryan.ruan@ericsson.com
%%% Purpose   : Obtain process information or kill the process, 
%%%             depending on the query type.
%%% Created   : Apr 9, 2013
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

-module(labrador_http_pid).

-behaviour(cowboy_http_handler).

%% Behaviour Callbacks
-export([init/3, handle/2, terminate/3]).

%% ===================================================================
%% Behaviour Callbacks
%% ===================================================================
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

%% ===================================================================
%% Inner Functions
%% ===================================================================
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
