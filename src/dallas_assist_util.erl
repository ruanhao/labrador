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

%% Path should be like: "js/main.js"
%% Path should not be like: "/js/main.js"
file(Path) ->
	AppName = application:get_application(),
	%% HEY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	dallas_assist:msg_trace(?LINE, process_info(self(), current_function), "App Name: ~p", [AppName]),
%% 	Priv = case code:priv_dir(dallas_assist) of
%% 			   {error,_} -> "priv";
%% 			   Priv0 -> Priv0
%% 		   end,
	{ok, CWD} = file:get_cwd(),
	dallas_assist:msg_trace(?LINE, process_info(self(), current_function), "CWD: ~p", [file:get_cwd()]),
	Priv = CWD ++ "/priv",
	dallas_assist:msg_trace(?LINE, process_info(self(), current_function), "Final Path: ~p", [filename:join(Priv, Path)]),
	file:read_file(filename:join(Priv, Path)).