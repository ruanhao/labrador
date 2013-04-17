%%%----------------------------------------------------------------------
%%% File    : dallas_assist_websocket_cni.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Continuously generate central node information which is 
%%%           supplied to Sparkline.
%%% Created : Apr 4, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_websocket_cni).
-behaviour(cowboy_websocket_handler).
-behaviour(cowboy_http_handler).
-define(INTERVAL, 2000).
-include("http.hrl").
-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3, websocket_info/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

handle(_Req, _State) ->
    exit(websockets_only).

terminate(_Reason, _Req, _State) ->
    exit(websockets_only).

websocket_init(_TransportName, Req, _Opts) ->
	timer:send_interval(?INTERVAL, central_node_info),
    {ok, Req, undefined_state}.

websocket_info(central_node_info, Req, State) -> 
	Reply = jsx:term_to_json(update()),
    {reply, {text, Reply}, Req, State};
websocket_info(Info, Req, State) ->
    io:format("Unhandled msg to ~p ~p\n", [?MODULE, Info]),
    {ok, Req, State}.

update() -> 
	CNode = dallas_assist_util:get_cnode(),
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
	
websocket_terminate(_Reason, _Req, _State) -> 
	ok.

websocket_handle(_Msg, Req, State) ->
	{ok, Req, State}.
