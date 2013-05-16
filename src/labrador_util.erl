%%%----------------------------------------------------------------------
%%% File      : labrador_util.erl
%%% Author    : ryan.ruan@ericsson.com
%%% Purpose   : Labrador utilities functions.
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

-module(labrador_util).

-compile(export_all).

-define(TABLE,      'CONFIGTABLE').

-define(CONFIGFILE, 'labrador.config').

-define(FMTOUTOK,      "~-30s ~-30s ==========> ok~n").
-define(FMTOUTOKNUM,   "~-30s ~-30w ==========> ok~n").
-define(FMTOUTFAIL,    "~-30s ~-30s ==========> fail~n").

-define(RETRY,      3).

%% ===================================================================
%% API Functions
%% ===================================================================
get_cnode() -> 
    case ets:lookup(?TABLE, central_node) of 
        [{central_node, N}] -> N;
        [] -> 
            error_logger:error_msg("Miss Central Node Configuration, Use node() Instead", []),
            node()
    end.

get_config(Key, Default) -> 
    case ets:lookup(?TABLE, Key) of 
        [{Key, V}] -> V;
        [] -> Default
    end. 

get_priv_path() -> 
    {ok, Cwd} = file:get_cwd(), 
    filename:join(Cwd, "priv").

%% Path should be like:     "js/main.js"
%% Path should not be like: "/js/main.js"
file(Path) ->
    Priv = get_config(priv, "."),
    file:read_file(filename:join(Priv, Path)).

consult_config() ->
    case file:consult(?CONFIGFILE) of
        {ok, CL} -> CL;
        _        -> exit("Config parsing fails")
    end.

create_config_table() ->
    ets:new(?TABLE, [set, public, named_table, {keypos, 1}]).

inflate_config_table(ConfigList) ->
    ets:insert(?TABLE, {priv, labrador_util:get_priv_path()}),
    [ ets:insert(?TABLE, {K, V}) || {K, V} <- ConfigList ].

setup_erlang_cluster(CNode) ->
    case net_adm:ping(CNode) of
        pong ->
            io:format(?FMTOUTOK,   ["Connecting to central node", CNode]),
            connect_nodes(CNode),
            ets:insert(?TABLE, {nodes, nodes(connected)});
        pang ->
            io:format(?FMTOUTFAIL, ["Connecting to central node", CNode]),
            exit("Central node unavailable")
    end.

connect_nodes(CNode) ->  
    Nodes = rpc:call(CNode, erlang, nodes, []), 
    connect_nodes(Nodes, [], 1). 

connect_nodes([], [], _) ->  
    io:format("All nodes connected~n", []);
connect_nodes([], Fails, ?RETRY) ->  
    io:format("These nodes can not be connected: ~w~n", [Fails]);
connect_nodes([], Fails, Retry) ->  
    connect_nodes(Fails, [], Retry + 1); 
connect_nodes([ H | T ], Fails, Retry) ->  
    Flag = net_kernel:connect_node(H),
    case Flag of  
        true ->  
            io:format(?FMTOUTOK,   ["Connecting to node", H]), 
            connect_nodes(T, Fails, Retry);
        _    ->  
            io:format(?FMTOUTFAIL, ["Connecting to node", H]), 
            connect_nodes(T, [ H | Fails ], Retry)
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

set_cluster_ticktime() -> 
    ClusterTick = rpc:call(get_cnode(), net_kernel, get_net_ticktime, []), 
    LocalTick   = case ClusterTick of 
                      {ongoing_change_to, NT} -> NT; 
                      ignored                 -> 60;
                      NT                      -> NT
                  end, 
    net_kernel:set_net_ticktime(LocalTick), 
    io:format(?FMTOUTOKNUM, ["Seting net tick time to", LocalTick]).
