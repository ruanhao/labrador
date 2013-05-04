%%%----------------------------------------------------------------------
%%% File    : labrador_http_ni.erl
%%% Author  : Hao Ruan <ryan.ruan@ericsson.com>
%%% Purpose : Update nodes information to generate menu on web page.
%%% Created : Apr 3, 2013
%%%----------------------------------------------------------------------
-module(labrador_http_ni).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
%% 	Nodes = ['dallas0@dls937-1', 
%% 			 'dallas1@dls937-1', 
%% 			 'dallas2@dls937-1', 
%% 			 'dallas0@dls937-2', 
%% 			 'dallas0@dls937-3', 
%% 			 'dallas0@dls937-4', 
%% 			 'dallas1@dls937-4', 
%% 			 'dallas0@dls937-5', 
%% 			 'dallas1@dls937-5'],
	Nodes = labrador_util:get_config(nodes, nodes(connected)), 
	Body = jsx:term_to_json(parse(Nodes)),
	Headers = [{<<"Content-Type">>, <<"application/json">>}],
	{ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

%% parse is used to output: 
%% [{'dls2300-1', [dallas0, dallas1, dallas2]}, 
%%	{'dls2300-2', [dallas0]}, 
%%	{'dls2300-3', [dallas0]}, 
%%	{'dls2300-4', [dallas0, dallas1]}, 
%%	{'dls2300-5', [dallas0, dallas1]}],
parse(Nodes) -> 
	NStrList = [atom_to_list(N) || N <- Nodes],
	{HNTL, HL} = parse1(NStrList, [], []),
	NodesAtomList = [[list_to_atom(Node) || {Host1, Node} <- HNTL, Host1 =:= Host] || Host <- HL],
	HostAtomList = [list_to_atom(H) || H <- HL],
	lists:zip(HostAtomList, NodesAtomList).

%% parse1 is used to output: 
%% {[{Host1, Node1}, {Host1, Node2}, {Host1, Node1}, ...]
%% [Host1, Host2, Host3, ...]}
parse1([], HNTL, HL) -> 
	{lists:reverse(HNTL), lists:reverse(HL)};
parse1([H | T], HostNodeTupleList, HostList) ->
	{Host, Node} = parse2(H, []),
	IsMbr = lists:member(Host, HostList),
	if 
		IsMbr -> 
			parse1(T, [{Host, Node} | HostNodeTupleList], HostList);
		true  -> 
			parse1(T, [{Host, Node} | HostNodeTupleList], [Host | HostList])
	end.

%% parse2 is used to output: {Host, Node}
parse2([H | T], Node) ->
	case H of
		$@ -> {T, lists:reverse(Node)};
		C  -> parse2(T, [C | Node])
	end.
