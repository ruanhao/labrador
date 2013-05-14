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
%% 			error_logger:error_msg("No Configuration For ~w", [Key]),
%% 			exit("Configuration Fail")
	end. 

%% Path should be like: "js/main.js"
%% Path should not be like: "/js/main.js"
file(Path) ->
	AppName = application:get_application(),
	%% HEY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	labrador:msg_trace(?LINE, process_info(self(), current_function), "App Name: ~p", [AppName]),
%% 	Priv = case code:priv_dir(labrador) of
%% 			   {error,_} -> "priv";
%% 			   Priv0 -> Priv0
%% 		   end,
	{ok, CWD} = file:get_cwd(),
	labrador:msg_trace(?LINE, process_info(self(), current_function), "CWD: ~p", [file:get_cwd()]),
	Priv = CWD ++ "/priv",
	labrador:msg_trace(?LINE, process_info(self(), current_function), "Final Path: ~p", [filename:join(Priv, Path)]),
	file:read_file(filename:join(Priv, Path)).
