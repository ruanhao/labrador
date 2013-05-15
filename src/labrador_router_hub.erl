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

-behaviour(gen_server).

-record(state, {}).

-define(SERVER, ?MODULE).

-define(DFLTIP, "127.0.0.1").

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
    IP0            = labrador_util:get_config(ip,              ?DFLTIP),
    NumAcceptors   = labrador_util:get_config(num_acceptors,   16),
    IP             = labrador_util:get_ip(IP0), 
    Localhost      = net_adm:localhost(),

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
    ConfigList = labrador_util:consult_config(),
    labrador_util:create_config_table(), 
    labrador_util:inflate_config_table(ConfigList), 
    %% configuration table should be inflated first,
    %% then we can do other setup
    labrador_util:set_cluster_ticktime(), 
    labrador_util:setup_erlang_cluster(labrador_util:get_cnode()).
