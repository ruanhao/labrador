%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.

-module(labrador).
-author('SMELLS LIKE BEAM SPIRIT').
-modified('Date: 2013/04/03').
-modified_by('ryan.ruan@ericsson.com').
-revision('Revision: 2.1').
-export([start/0, stop/0, msg_trace/4]).

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
	ensure_started(ranch),
    ensure_started(cowboy),
    ensure_started(jsx),
    application:start(labrador).

stop() ->
    application:stop(labrador).

msg_trace(LineNum, ProcInfo, DebugStr, DebugVals) ->
  ModName = element(1, element(2, ProcInfo)),
  FunName = element(2, element(2, ProcInfo)),
  FinalFormatStr = "Fun: " ++ atom_to_list(FunName) ++ " -- " ++ DebugStr ++ " -- #Module: ~p, @Line: ~p~n",
  Parameters = DebugVals ++ [ModName, LineNum],                                             
  io:format(FinalFormatStr, Parameters). 


		
