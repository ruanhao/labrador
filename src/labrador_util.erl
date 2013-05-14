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

-define(TABLE, ctable).

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
