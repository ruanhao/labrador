%%%----------------------------------------------------------------------
%%% File    : dallas_assist_router_hub.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com> (Acknowlegement to BigWig)
%%% Purpose : Handle request dispatchment.
%%% Created : Apr 3, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_router_hub).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DFLTIP, "127.0.0.1").
-define(RETRY, 3).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
  	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

dispatch_rules() ->
    %% {Host, list({Path, Handler, Opts})}
    [{'_', [{"/",                       	dallas_assist_http_static, [<<"html">>,<<"index.html">>]}, 
			{"/static",     dallas_assist_http_static, []}, 
			{"/ni",      	dallas_assist_http_ni, []}, 
			{"/cni",      	dallas_assist_http_cni, []}, 
			{"/pid",        dallas_assist_http_pid, []}, 
			{"/etop",       dallas_assist_websocket_etop, []}, 
			{"/cnis",      		dallas_assist_websocket_cni, []}, 
			{'_',                       dallas_assist_http_catchall, []}]}].

init([]) ->
	io:format("~nStarting Dallas Assist ... ~n", []), 
	ensure_config_right(),
    Port            = dallas_assist_util:get_config(port, 40829),
    IP0             = dallas_assist_util:get_config(ip, "127.0.0.1"),
    NumAcceptors    = dallas_assist_util:get_config(num_acceptors, 16),
	%% Cowboy Specifications
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	%% cowboy:start_listener(http, NumAcceptors,
	%% 					  cowboy_tcp_transport, [{port, Port}],
	%% 					  cowboy_http_protocol, [{dispatch, dispatch_rules()}]),

  cowboy:start_http(my_http_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, cowboy_router:compile(dispatch_rules())}]}]),

	{LH, IP} = localhost_ip(IP0), 
    error_logger:info_msg("Dallas Assist is ready on: ~s~n"
			 		 	  "Listening on http://~s:~B/~n", [LH, IP,Port]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
ensure_config_right() -> 
	case file:consult("dallas.config") of 
		{ok, ConfigList} -> 
			ets:new(ctable, [set, public, named_table, {keypos, 1}]),
			[begin 
				 case K of 
					 central_node -> 
						 case net_adm:ping(V) of 
							 pong -> %% this hidden node is connected to central node :)
                                 io:format("Connecting to node ~w ==========> ok~n", [V]), 
								 ets:insert(ctable, {K, V}),
								 connect_nodes(V),
                                                                 ets:insert(ctable, {nodes, nodes(connected)});
							 pang -> 
								 exit("Central Node In Config Is Wrong")
						 end;
					 _ -> 
						 ets:insert(ctable, {K, V})
				 end
			 end || {K, V} <- ConfigList];
		_ -> 
			exit("Wrong Config")
	end.

connect_nodes(CNode) -> 
	Nodes = rpc:call(CNode, erlang, nodes, []), 
	connect_nodes(Nodes, [], 0).

connect_nodes([], [], _) -> 
	ok;
connect_nodes([], Fails, ?RETRY) -> 
	io:format("These nodes can not be connected: ~w~n", [Fails]);
connect_nodes([], Fails, Retry) -> 
	connect_nodes(Fails, [], Retry + 1); 
connect_nodes([H | T], Fails, Retry) -> 
	Flag = net_kernel:connect_node(H),
	case Flag of 
		true -> io:format("Connecting to node ~w ==========> ok~n", [H]), 
				connect_nodes(T, Fails, Retry);
		_ -> io:format("Connecting to node ~w ==========> nok~n", [H]), 
			 connect_nodes(T, [H | Fails], Retry)
	end.

localhost_ip(DefaultIP) -> 
	LocalHost = net_adm:localhost(), 
	case os:cmd("nslookup " ++ LocalHost ++ " | grep " ++ "\"can't find\"") of 
		[] -> 
			Addr = os:cmd("nslookup " ++ LocalHost ++ " | tail -n 2"), 
			[_, IP] = string:tokens(Addr, "\n "), 
			{LocalHost, IP};
		_ -> 
			{LocalHost, DefaultIP}
	end.
