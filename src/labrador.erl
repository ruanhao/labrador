%%%----------------------------------------------------------------------
%%% File      : labrador.erl
%%% Author    : SMELLS LIKE BEAM SPIRIT
%%% Modifier  : ryan.ruan@ericsson.com
%%% Purpose   : Bootstrap module.
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

-module(labrador).

%% API Functions
-export([start/0, stop/0, msg_trace/4]).

%% ===================================================================
%% API Functions
%% ===================================================================
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

%% ===================================================================
%% Inner Functions
%% ===================================================================
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
