%%%----------------------------------------------------------------------
%%% File      : labrador_router_hub.erl
%%% Author    : SMELLS LIKE BEAM SPIRIT
%%% Modifier  : ryan.ruan@ericsson.com
%%% Purpose   : Handle requests routing.
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

-module(labrador_router_hub).

-define(SERVER, ?MODULE).

-define(DFLTIP, "127.0.0.1").

-define(RETRY, 3).

-record(state, {}).

-behaviour(gen_server).

%% API Function
-export([start_link/0]).

%% Behaviour Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Functions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% Behaviour Callbacks
%% ------------------------------------------------------------------
init([]) ->
    io:format("~nstarting labrador ... ~n", []), 
    ensure_config_right(),
    Port           = labrador_util:get_config(port,            40829),
    IP0            = labrador_util:get_config(ip,              "127.0.0.1"),
    NumAcceptors   = labrador_util:get_config(num_acceptors,   16),
    Localhost      = net_adm:localhost(),
    IP             = localhost_ip(IP0), 

    %% ------------------------------------------------------------------
    %% Cowboy Specifications
    %% cowboy:start_http(Ref, NbAcceptors, TransOpts, ProtoOpts) 
    %% ------------------------------------------------------------------
    cowboy:start_http(labrador_listener, 
                      NumAcceptors, 
                      [{port, Port}], 
                      [ 
                       {env, [ 
                              {dispatch, cowboy_router:compile(dispatch_rules())} ]} ]),

    error_logger:info_msg("labrador is ready on: ~s~n"
                          "listening on http://~s:~B/~n", [Localhost, IP,Port]),
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
%% Inner Functions
%% ------------------------------------------------------------------
dispatch_rules() ->
    %% {Host, list({Path, Handler, Opts})}
    [{'_', [{"/",                 labrador_http_static, [<<"html/index.html">>]}, 
            {"/static/[...]",     labrador_http_static, []}, 
            {"/ni",         labrador_http_ni, []}, 
            {"/cni",        labrador_http_cni, []}, 
            {"/pid",        labrador_http_pid, []}, 
            {"/etop",       labrador_websocket_etop, []}, 
            {"/cnis",       labrador_websocket_cni, []}, 
            {'_',           labrador_http_catchall, []}]}].


ensure_config_right() -> 
    labrador:msg_trace(?LINE, process_info(self(), current_function), "app name: ~p", [application:get_application()]),
    labrador:msg_trace(?LINE, process_info(self(), current_function), "cwd: ~p", [file:get_cwd()]),
    case file:consult("labrador.config") of 
        {ok, ConfigList} -> 
            ets:new(ctable, [set, public, named_table, {keypos, 1}]),
            [begin 
                 case K of 
                     central_node -> 
                         case net_adm:ping(V) of 
                             pong -> %% this hidden node has been connected to central node :)
                                 io:format("Connecting to central node ~w ==========> ok~n", [V]), 
                                 ets:insert(ctable, {K, V}),
                                 connect_nodes(V), 
                                 ets:insert(ctable, {nodes, nodes(connected)});
                             pang -> 
                                 io:format("Connecting to central node ~w ==========> fail~n", [V]), 
                                 exit("Central node unavailable")
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
    io:format("All nodes connected~n", []);
connect_nodes([], Fails, ?RETRY) -> 
    io:format("These nodes can not be connected: ~w~n", [Fails]);
connect_nodes([], Fails, Retry) -> 
    connect_nodes(Fails, [], Retry + 1); 
connect_nodes([H | T], Fails, Retry) -> 
    Flag = net_kernel:connect_node(H),
    case Flag of 
        true -> 
            io:format("Connecting to node ~w ==========> ok~n", [H]), 
            connect_nodes(T, Fails, Retry);
        _    -> 
            io:format("Connecting to node ~w ==========> fail~n", [H]), 
            connect_nodes(T, [H | Fails], Retry)
    end.

localhost_ip(DefaultIP) -> 
    
