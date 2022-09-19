%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 19:52
%%%-------------------------------------------------------------------
-module(routherd).
-author("frane").

%% API
-export([get_status/1, start/2, stop/1, test/0, test2/0, sweden/0, germany/0, usa/0, france/0, international/0]).


start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Interfaces = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Interfaces, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Interfaces, Table, Map).

router(Name, N, Hist, Interfaces, Table, Map) ->
  receive
    {add, Node, Pid} ->
      Ref = erlang:monitor(process, Pid),
      Interfaces1 = intf:add(Node, Ref, Pid, Interfaces),
      io:format("~w:  Connected to ~w pid ~w~n", [Name, Node, Pid]),
      router(Name, N, Hist, Interfaces1, Table, Map);

    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Interfaces),
      erlang:demonitor(Ref),
      io:format("~w:  Disconnected from ~w~n", [Name, Node]),
      Interfaces1 = intf:remove(Node, Interfaces),
      router(Name, N, Hist, Interfaces1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Interfaces),
      io:format("~w:  exit received from ~w~n", [Name, Down]),
      Node = intf:name(Ref, Interfaces),
      InterfacesNew = intf:remove(Down, Interfaces),
      intf:broadcast({links, Name, N, intf:list(Interfaces)}, InterfacesNew),
      MapNew = map:update(Node, [], Map),
      TableNew = dijkstra:table(intf:list(InterfacesNew), MapNew),
      router(Name, N + 1, Hist, InterfacesNew, TableNew, MapNew);

    {status, From} ->
      From ! {status, {Name, N, Hist, Interfaces, Table, Map}},
      router(Name, N, Hist, Interfaces, Table, Map);

    {links, Node, R, Links} ->
      io:format("~w:  Got links ~w~n", [Name, {Node, R, Links}]),
      case hist:update(Node, R, Hist) of
        {new, HistNew} ->
          intf:broadcast({links, Node, R, Links}, Interfaces),
          MapNew = map:update(Node, Links, Map),
          TableNew = dijkstra:table(intf:list(Interfaces), MapNew),
          router(Name, N, HistNew, Interfaces, TableNew, MapNew);
        old ->
          router(Name, N, Hist, Interfaces, Table, Map)
      end;

    update ->
      io:format("~w:  Got update~n", [Name]),
      Table1 = dijkstra:table(intf:list(Interfaces), Map),
      router(Name, N, Hist, Interfaces, Table1, Map);

    broadcast ->
      io:format("~w:  Got broadcast~n", [Name]),
      Message = {links, Name, N, intf:list(Interfaces)},
      intf:broadcast(Message, Interfaces),
      router(Name, N + 1, Hist, Interfaces, Table, Map);

    flood ->
      intf:broadcast({flood, [Name]}, Interfaces),
      intf:broadcast({links, N, Hist, Interfaces, Table, Map}, Interfaces),
      router(Name, N + 1, Hist, Interfaces, Table, Map);

    {flood, Flooded} ->
      case lists:member(Name, Flooded) of
        true -> router(Name, N, Hist, Interfaces, Table, Map);
        false ->
          Message = {links, Name, N, intf:list(Interfaces)},
          intf:broadcast({flood, [Name | Flooded]}, Interfaces),
          intf:broadcast(Message, Interfaces),
          router(Name, N + 1, Hist, Interfaces, Table, Map)
      end;

    {route, Name, From, Message} ->
      io:format("~w: received message from ~w : ~w ~n", [Name, From, Message]),
      router(Name, N, Hist, Interfaces, Table, Map);

    {route, To, From, Message} ->
      io:format("~w: routing message (~w)~n", [Name, Message]),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case intf:lookup(Gw, Interfaces) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Interfaces, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Interfaces, Table, Map);

    stop ->
      io:format("~w:  Got stop~n", [Name]),
      ok
  end.

get_status(Pid) ->
  Pid ! {status, self()},
  receive
    {status, {Name, N, Hist, Interfaces, Table, Map}} ->
      io:format("Status from ~w with name ~w~nMessages: ~w~nHist: ~w~nInterfaces: ~w~nTable: ~w~nMap: ~w~n", [Pid, Name, N, Hist, Interfaces, Table, Map])
  end.

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

test2() ->
  start(r1, stockholm),
  start(r2, lund),
  start(r3, malmo),
  start(r4, arlanda),
  start(r5, kiruna),
  r1 ! {add, lund, {r2, 'sweden@130.123.112.23'}},
  r1 ! {add, arlanda, {r4, 'sweden@130.123.112.23'}},
  r2 ! {add, malmo, {r3, 'sweden@130.123.112.23'}},
  r3 ! {add, arlanda, {r4, 'sweden@130.123.112.23'}},
  r3 ! {add, kiruna, {r5, 'sweden@130.123.112.23'}},
  r4 ! {add, kiruna, {r5, 'sweden@130.123.112.23'}},
  r5 ! {add, stockholm, {r1, 'sweden@130.123.112.23'}}.

sweden() ->
  start(s1, stockholm),
  start(s2, lund),
  start(s3, malmo),
  s1 ! {add, lund, {s2, 'sweden@localhost'}},
  s2 ! {add, malmo, {s3, 'sweden@localhost'}},
  s3 ! {add, stockholm, {s1, 'sweden@localhost'}},
  s1 ! flood.

germany() ->
  start(g1, berlin),
  start(g2, hamburg),
  start(g3, cologne),
  g1 ! {add, hamburg, {g2, 'germany@localhost'}},
  g2 ! {add, cologne, {g3, 'germany@localhost'}},
  g3 ! {add, berlin, {g1, 'germany@localhost'}},
  g1 ! flood.

usa() ->
  start(u1, washington),
  start(u2, atlanta),
  start(u3, newyork),
  u1 ! {add, atlanta, {u2, 'usa@localhost'}},
  u2 ! {add, newyork, {u3, 'usa@localhost'}},
  u3 ! {add, washington, {u1, 'usa@localhost'}},
  u1 ! flood.

france() ->
  start(f1, paris),
  start(f2, lyon),
  start(f3, marseille),
  f1 ! {add, lyon, {f2, 'france@localhost'}},
  f2 ! {add, marseille, {f3, 'france@localhost'}},
  f3 ! {add, paris, {f1, 'france@localhost'}},
  f1 ! flood.

international() ->
  {s1, 'sweden@localhost'} ! {add, cologne, {g3, 'germany@localhost'}},
  {g1, 'germany@localhost'} ! {add, newyork, {u3, 'usa@localhost'}},
  {u1, 'usa@localhost'} ! {add, marseille, {f3, 'france@localhost'}},
  {f1, 'france@localhost'} ! {add, malmo, {s3, 'sweden@localhost'}}.
