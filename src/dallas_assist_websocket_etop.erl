%%%----------------------------------------------------------------------
%%% File    : dallas_assist_websocket_etop.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Continuously generate Erlang top information.
%%% Created : Apr 7, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_websocket_etop).
-behaviour(cowboy_websocket_handler).
-behaviour(cowboy_http_handler).
-define(INTERVAL, 3000).
-define(DEFAULT_LINES, 100).
-record(opts, {node = node(), accum = false, intv = ?INTERVAL, lines = ?DEFAULT_LINES, sort = reductions, accum_tab}).
-include("observer_backend.hrl").
-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3, websocket_info/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

handle(_Req, _State) ->
    exit(websockets_only).

terminate(_Reason, _Req, _State) ->
    exit(websockets_only).

websocket_init(_TransportName, Req, _Opts) ->
	Opts = opts(Req),
	check_connectivity(Opts),
	timer:send_after(0, update),
	timer:send_interval(Opts#opts.intv, update),
    {ok, Req, Opts}.

websocket_info(update, Req, Opts) -> 
	Reply = jsx:term_to_json(update(Opts)),
    {reply, {text, Reply}, Req, Opts};
websocket_info(Info, Req, State) ->
    io:format("Unhandled msg to ~p ~p\n", [?MODULE, Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> 
	ok.

websocket_handle(Msg, Req, Opts) ->
	{text, MsgBin} = Msg,
	ParaAtom = list_to_atom(binary_to_list(MsgBin)),
	OptUpdated = config(ParaAtom, Opts),
	timer:send_after(0, update),
 	{ok, Req, OptUpdated}.

config(Para, Opts) -> 
	case Para of 
		reductions -> Opts#opts{sort = reductions};
		memory -> Opts#opts{sort = memory};
		msg_q -> Opts#opts{sort = msg_q};
		accum -> Opts#opts{accum = true};
		no_accum -> Opts#opts{accum = false}
	end.

update(#opts{node = Node} = Opts) -> 
	Pid = spawn_link(Node,observer_backend,etop_collect,[self()]),
	EtopInfo = receive 
				   {Pid,I} -> I
				   after 1000 -> exit(connection_lost)
			   end,
	[node_info(Opts, EtopInfo), proc_infos(Opts, EtopInfo)].

mem_property(MemInfo, Key) -> 
	case lists:keyfind(Key, 1, MemInfo) of 
		{Key, V} -> V;
		false -> 'NaN'
	end.

node_info(#opts{node = Node}, EtopInfo) -> 
	#etop_info{n_procs = Procs, 
			   run_queue = Rq, 
			   wall_clock = {_, Wc}, 
			   runtime = {_, Rt}} = EtopInfo,
	Cpu = try round(100*Rt/Wc)
		  catch _:_ -> 0
		  end,
	MemInfo = rpc:call(Node, erlang, memory, []), 
	Tot = mem_property(MemInfo, total),
	Binary = mem_property(MemInfo, binary),
	Processes = mem_property(MemInfo, processes),
	Code = mem_property(MemInfo, code),
	Atom = mem_property(MemInfo, atom), 
	Ets = mem_property(MemInfo, ets),
	{node_info, [Cpu, Procs, Rq, Tot, Binary, Processes, Code, Atom, Ets]}.

proc_infos(Opts, EtopInfo) -> 
	ProcInfos = EtopInfo#etop_info.procinfo,
	Tag = get_tag(Opts#opts.sort),
	PIs1 = if Opts#opts.accum ->
				  ProcInfos;
			  true ->
				  AccumTab = Opts#opts.accum_tab,
				  [begin 
					   ets:insert(AccumTab, PI),
					   #etop_proc_info{pid = Pid, reds = Reds} = PI,
					   case ets:lookup(AccumTab, Pid) of 
						   [#etop_proc_info{reds = OldReds}] -> 
							   PI#etop_proc_info{reds = Reds - OldReds};
						   [] -> 
							   PI
					   end 
				   end || PI <- ProcInfos]
		   end,
	PIs2 = lists:reverse(lists:keysort(Tag,PIs1)),
	{proc_info, lists:sublist(PIs2,Opts#opts.lines)}.

get_tag(memory) -> #etop_proc_info.mem;
get_tag(reductions) -> #etop_proc_info.reds;
get_tag(msg_q) -> #etop_proc_info.mq.

check_connectivity(Opts) -> 
	Node = Opts#opts.node,
	case net_adm:ping(Node) of
		pang when Node /= node() ->
			error("Node Connect Failed.");
		pong ->
			case check_runtime_tools_vsn(Node) of
				ok -> ok;
				{error, Reason} -> error(Reason)
			end
	end.

check_runtime_tools_vsn(Node) ->
	case rpc:call(Node,observer_backend,vsn,[]) of
		{ok, Vsn} -> check_vsn(Vsn);
		_ -> {error, "Faulty Version Of Runtime Tools On Remote Node."}
	end.

check_vsn(_Vsn) -> ok.

opts(Req) -> 
	{Qs, _} = cowboy_req:qs_val(<<"node">>, Req),
	MNode = list_to_atom(binary_to_list(Qs)),
	AccumTab = ets:new(accum_tab, [set,public,{keypos,#etop_proc_info.pid}]),
	#opts{node = MNode, 
		  accum_tab = AccumTab, 
		  intv = dallas_assist_util:get_config(interval, ?INTERVAL), 
		  lines = dallas_assist_util:get_config(lines, ?DEFAULT_LINES)}.
