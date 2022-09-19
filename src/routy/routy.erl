%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 19:52
%%%-------------------------------------------------------------------
-module(routy).
-author("frane").

%% API
-export([get_status/1, start/2, stop/1, test/0, broadcast/0]).


start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      io:format("~w:  Connected to ~w pid ~w~n", [Name, Node, Pid]),
      router(Name, N, Hist, Intf1, Table, Map);
    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      io:format("~w:  Disconnected from ~w~n", [Name, Node]),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);
    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w:  exit recived from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);
    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);
    {links, Node, R, Links} ->
      io:format("~w:  Got links ~w~n", [Name, {Node, R, Links}]),
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          intf:broadcast({links, Node, R, Links}, Intf),
          MapNew = map:update(Node, Links, Map),
          TableNew = dijkstra:table(intf:list(Intf), MapNew),
          router(Name, N, Hist1, Intf, TableNew, MapNew);
        old ->
          router(Name, N, Hist, Intf, Table, Map)
      end;
    update ->
      io:format("~w:  Got update~n", [Name]),
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);
    broadcast ->
      io:format("~w:  Got broadcast~n", [Name]),
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N + 1, Hist, Intf, Table, Map);
    {broadcast_all, Broadcasted} ->
      case lists:member(Name, Broadcasted) of
        true -> router(Name, N, Hist, Intf, Table, Map);
        false ->
          Message = {links, Name, N, intf:list(Intf)},
          intf:broadcast({broadcast_all, [Name | Broadcasted]}, Intf),
          intf:broadcast(Message, Intf),
          router(Name, N + 1, Hist, Intf, Table, Map)
      end;
    {route, Name, From, Message} ->
      io:format("~w: received message from ~w : ~w ~n", [Name, From, Message]),
      router(Name, N, Hist, Intf, Table, Map);
    {route, To, From, Message} ->
      io:format("~w: routing message (~w)~n", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);
    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);
    stop ->
      io:format("~w:  Got stop~n", [Name]),
      ok
  end.

get_status(Pid) ->
  Pid ! {status, self()},
  receive
    {status, {Name, N, Hist, Intf, Table, Map}} ->
      io:format("Status from ~w with name ~w~nMessages: ~w~nHist: ~w~nIntf: ~w~nTable: ~w~nMap: ~w~n", [Pid, Name, N, Hist, Intf, Table, Map])
  end.

broadcast() ->
  r1 ! broadcast,
  r2 ! broadcast,
  r3 ! broadcast,
  r4 ! broadcast.

test() ->
  start(r1, stockholm),
  start(r2, lund),
  start(r3, malmo),
  start(r4, arlanda),
  r1 ! {add, lund, {r2, 'sweden@130.123.112.23'}},
  r1 ! {add, malmo, {r3, 'sweden@130.123.112.23'}},
  r2 ! {add, malmo, {r3, 'sweden@130.123.112.23'}},
  r3 ! {add, arlanda, {r4, 'sweden@130.123.112.23'}},
  r4 ! {add, lund, {r2, 'sweden@130.123.112.23'}},
  r3 ! {add, stockholm, {r1, 'sweden@130.123.112.23'}}.