%%%----------------------------------------------------------------------
%%% File    : dallas_assist.erl
%%% Author  : BigWig
%%% Purpose : Startup file.
%%% Created : Apr 3, 2013
%%%----------------------------------------------------------------------
-module(dallas_assist).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
	ensure_started(crypto),
	ensure_started(sasl),
    ensure_started(cowboy),
    ensure_started(jsx),
    application:start(dallas_assist).

stop() ->
    application:stop(dallas_assist).

%% msg_trace(LineNum, ProcInfo, DebugStr, DebugVals) ->
%%   ModName = element(1, element(2, ProcInfo)),
%%   FunName = element(2, element(2, ProcInfo)),
%%   FinalFormatStr = "Fun: " ++ atom_to_list(FunName) ++ " -- " ++ DebugStr ++ " -- #Module: ~p, @Line: ~p~n",
%%   Parameters = DebugVals ++ [ModName, LineNum],                                             
%%   io:format(FinalFormatStr, Parameters). 


		
