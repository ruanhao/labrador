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
    error_logger:info_msg("starting labrador ... ~n~n", []), 
    init_config(),
    Port           = labrador_util:get_config(port,            40829),
    IP0            = labrador_util:get_config(ip,              "127.0.0.1"),
    NumAcceptors   = labrador_util:get_config(num_acceptors,   16),
    Localhost      = net_adm:localhost(),
    IP             = get_ip(IP0), 

    %% ------------------------------------------------------------------
    %% Cowboy Specifications
    %% cowboy:start_http(Ref, NbAcceptors, TransOpts, ProtoOpts) 
    %% ------------------------------------------------------------------
    cowboy:start_http(labrador_listener, 
                      NumAcceptors, 
                      [{port, Port}], 
                      [{env, [{dispatch, cowboy_router:compile(dispatch_rules())}]}]),

    error_logger:info_msg("labrador is rocking on: ~s, "
                          "please visit http://~s:~B/~n", [Localhost, IP,Port]),
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
    [{'_', [{"/",               labrador_http_static,    [<<"html/index.html">>]}, 
            {"/static/[...]",   labrador_http_static,    []}, 
            {"/ni",             labrador_http_ni,        []}, 
            {"/cni",            labrador_http_cni,       []}, 
            {"/pid",            labrador_http_pid,       []}, 
            {"/etop",           labrador_websocket_etop, []}, 
            {"/cnis",           labrador_websocket_cni,  []}, 
            {'_',               labrador_http_catchall,  []}]}].


init_config() -> 
    ConfigList = 
    case file:consult("labrador.config") of 
        {ok, CL} -> CL;
        _        -> exit("Config parsing fails")
    end,
    ets:new(ctable, [set, public, named_table, {keypos, 1}]),
    ets:insert(ctable, {priv, labrador_util:get_priv_path()}),
    [ets:insert(ctable, {K, V}) || {K, V} <- ConfigList],
    CNode = labrador_util:get_cnode(),
    case net_adm:ping(CNode) of 
        pong -> 
            io:format("Connecting to central node ~w ==========> ok~n", [CNode]),
            connect_nodes(CNode),
            ets:insert(ctable, {nodes, nodes(connected)});
        pang -> 
            io:format("Connecting to central node ~w ==========> fail~n", [CNode]),
            exit("Central node unavailable")
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

get_ip(DefaultIP) -> 
    Priv   = labrador_util:get_priv_path(), 
    Script = filename:join(Priv, "scripts/ship.sh"), 
    Ref    = make_ref(), 
    Parent = self(), 
    Worker = proc_lib:spawn(fun() -> 
                                Info = os:cmd(Script), 
                                Parent ! {Ref, Info}
                            end), 
    receive 
        {Ref, Info} -> Info
    after 5000 -> 
        %% if msg happens to come here now, i just discard it. 
        %% no need to flush the process msg queue, because
        %% the function handle_info/2 can help us.
        exit(Worker, kill), 
        DefaultIP
    end.
    
    
