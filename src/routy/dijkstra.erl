%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 16:44
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("frane").

%% API
-export([table/2, route/2]).

entry(_, []) ->
  0;
entry(Node, [{Node, Hops, _} | _]) ->
  Hops;
entry(Node, [_ | Rest]) ->
  entry(Node, Rest).

replace(Node, Hops, Gateway, Sorted) ->
  case lists:keytake(Node, 1, Sorted) of
    {value, _, Filtered} -> insert_sorted(Node, Hops, Gateway, Filtered);
    false -> Sorted
  end.

update(Node, Hops, Gateway, Sorted) ->
  CurrentHops = entry(Node, Sorted),
  if
    CurrentHops > Hops -> replace(Node, Hops, Gateway, Sorted);
    true -> Sorted
  end.

insert_sorted(Node, Hops, Gateway, []) ->
  [{Node, Hops, Gateway}];
insert_sorted(Node, Hops, Gateway, [{_Node, _Hops, _Gateway} | Rest]) when Hops > _Hops ->
  [{_Node, _Hops, _Gateway} | insert_sorted(Node, Hops, Gateway, Rest)];
insert_sorted(Node, Hops, Gateway, Rest) ->
  [{Node, Hops, Gateway} | Rest].

iterate([], _, Table) ->
  Table;
iterate([{_, inf, _} | _], _, Table) ->
  Table;
iterate([{Node, Hops, Gateway} | Rest], Map, Table) ->
  Reachable = map:reachable(Node, Map),
  Sorted = lists:foldl(fun(Elem, List) ->
    update(Elem, Hops + 1, Gateway, List)
                       end, Rest, Reachable),
  iterate(Sorted, Map, [{Node, Gateway} | Table]).

table(Gateways, Map) ->
  Nodes = map:all_nodes(Map),
  Infinite = lists:foldl(fun(Node, Acc) -> [{Node, inf, unknown} | Acc] end, [], Nodes),
  Sorted = lists:foldl(fun(Node, Acc) -> replace(Node, 0, Node, Acc) end, Infinite, Gateways),
  iterate(Sorted, Map, []).

route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {_, Gateway} -> {ok, Gateway};
    false -> notfound
  end.