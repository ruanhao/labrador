%%%----------------------------------------------------------------------
%%% File      : labrador_websocket_cni.erl
%%% Author    : ryan.ruan@ericsson.com
%%% Purpose   : Continuously generate central node information which is 
%%%             supplied to Sparkline.
%%% Created   : Apr 4, 2013
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

-module(labrador_websocket_cni).

-include("http.hrl").

-define(INTERVAL, 2000).

-behaviour(cowboy_websocket_handler).

-behaviour(cowboy_http_handler).

%% Behaviour Callbacks (cowboy_http_handler)
-export([init/3, handle/2, terminate/3]).

%% Behaviour Callbacks (cowboy_websocket_handler)
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3, websocket_info/3]).

%% ===================================================================
%% Behaviour Callbacks (cowboy_http_handler)
%% ===================================================================
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

handle(_Req, _State) ->
    exit(websockets_only).

terminate(_Reason, _Req, _State) ->
    exit(websockets_only).

%% ===================================================================
%% Behaviour Callbacks (cowboy_websocket_handler)
%% ===================================================================
websocket_init(_TransportName, Req, _Opts) ->
	timer:send_interval(?INTERVAL, central_node_info),
    {ok, Req, undefined_state}.

websocket_info(central_node_info, Req, State) -> 
	Reply = jsx:term_to_json(update()),
    {reply, {text, Reply}, Req, State};
websocket_info(Info, Req, State) ->
    io:format("Unhandled msg to ~p ~p\n", [?MODULE, Info]),
    {ok, Req, State}.

	
websocket_terminate(_Reason, _Req, _State) -> 
	ok.

websocket_handle(_Msg, Req, State) ->
	{ok, Req, State}.

%% ===================================================================
%% Inner Functions
%% ===================================================================
update() -> 
	CNode = labrador_util:get_cnode(),
	NProcs = length(rpc:call(CNode, erlang, processes, [])), 
	MemTot = rpc:call(CNode, erlang, memory, [total]),
	MemEts = rpc:call(CNode, erlang, memory, [ets]),
%% 	Pid = spawn_link(CNode,observer_backend,etop_collect,[self()]),
%% 	EtopInfo = receive 
%% 				   {Pid,I} -> I
%% 				   after 1000 -> exit(connection_lost)
%% 			   end,
%% 	#etop_info{wall_clock = {_, Wc}, 
%% 			   runtime = {_, Rt}} = EtopInfo,
%% 	Cpu = try round(100*Rt/Wc)
%% 		  catch _:_ -> 0
%% 		  end,
	[{nprocs, NProcs}, {memtot, MemTot}, {ets, MemEts}].