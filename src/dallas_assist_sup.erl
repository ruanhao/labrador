%%%----------------------------------------------------------------------
%%% File    : dallas_assist_sup.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>	
%%%           (Acknowlegement to BigWig)
%%% Purpose : Handle all unexpected URL.
%%% Created : Apr 3, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() -> 
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Http        = ?CHILD(dallas_assist_router_hub, worker),
    Specs       = [Http],
    {ok, {{one_for_one, 5, 10}, Specs}}.
