%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2022 18:58
%%%-------------------------------------------------------------------
-module(test).
-author("frane").

%% API
-export([bench/3, bench_parallel/4, bench/4]).

bench_parallel(N, Processes, Host, Port) ->
  Start = erlang:system_time(micro_seconds),
  bench_parallel_(N, Processes, Host, Port),
  {Sum, Max, Min} = aggregate_parallel(Processes, {0, 0, infinity}),
  Total = erlang:system_time(micro_seconds) - Start,
  Avg = Sum / Processes,
  io:format("--- Parallel Test ---~nN: ~w~nProcesses: ~w~nAvg: ~w~nSum: ~w~nMax: ~w~nMin: ~w~nTotal: ~w~n", [N, Processes, Avg, Sum, Max, Min, Total]).

bench_parallel_(N, Processes, Host, Port) ->
  if
    Processes == 0 ->
      ok;
    true ->
      spawn(test, bench, [N, Host, Port, self()]),
      bench_parallel_(N, Processes - 1, Host, Port)
  end.
bench(N, Host, Port, Pid) ->
  Time = bench(N, Host, Port),
  Pid ! {time, Time}.

aggregate_parallel(0, {Sum, Max, Min}) ->
  {Sum, Max, Min};
aggregate_parallel(Processes, {Sum, Max, Min}) ->
  receive
    {time, Time} ->
      if
        Time < Min ->
          aggregate_parallel(Processes - 1, {Sum + Time, Max, Time});
        Time > Max ->
          aggregate_parallel(Processes - 1, {Sum + Time, Time, Min});
        true ->
          aggregate_parallel(Processes - 1, {Sum + Time, Max, Min})
      end
  end.

bench(N, Host, Port) ->
  Start = erlang:system_time(micro_seconds),
  run(N, Host, Port),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.

run(N, Host, Port) ->
  if
    N == 0 ->
      ok;
    true ->
      request(Host, Port),
      run(N - 1, Host, Port)
  end.

request(Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}, {send_timeout, infinity}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("foo")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _} ->
      ok;
    {error, Error} ->
      io:format("test: Error: ~p~n", [Error])
  end,
  gen_tcp:close(Server).