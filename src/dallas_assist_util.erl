%%%----------------------------------------------------------------------
%%% File    : dallas_assist_util.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Utilities functions
%%% Created : Apr 8, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist_util).
-compile(export_all).
-define(TABLE, ctable).

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

file(Path) ->
	Priv = case code:priv_dir(bigwig) of
			   {error,_} -> "priv";
			   Priv0 -> Priv0
		   end,
	file:read_file(filename:join(Priv, Path)).