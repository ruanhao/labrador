%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
-module(labrador_app).

-author('SMELLS LIKE BEAM SPIRIT').
-modified('Date: 2013/04/03').
-modified_by('ryan.ruan@ericsson.com').
-revision('Revision: 2.1').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) -> 
	labrador_sup:start_link().

stop(_State) ->
    ok.
