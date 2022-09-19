%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 15:56
%%%-------------------------------------------------------------------
-module(map).
-author("frane").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
  [].

update(Node, Links, [{Node, _} | Rest]) ->
  [{Node, Links} | Rest];
update(Node, Links, []) ->
  [{Node, Links}];
update(Node, Links, [H | Rest]) ->
  [H | update(Node, Links, Rest)].

reachable(Node, [{Node, Links} | _]) ->
  Links;
reachable(_, []) ->
  [];
reachable(Node, [_ | Rest]) ->
  reachable(Node, Rest).

add_to_set(Elem, [Elem | Rest]) ->
  Rest;
add_to_set(Elem, []) ->
  [Elem];
add_to_set(Elem, [H | Rest]) ->
  [H | add_to_set(Elem, Rest)].

all_nodes(Map) ->
  DeepFlat = lists:map(fun({Node, Links}) -> [Node | Links] end, Map),
  Flat = lists:append(DeepFlat),
  lists:uniq(Flat).