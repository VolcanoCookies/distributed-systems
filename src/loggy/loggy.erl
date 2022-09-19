%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 14:51
%%%-------------------------------------------------------------------
-module(loggy).
-author("frane").

%% API
-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(vec:clock(Nodes), orderqueue:new()).

loop(Clock, Queue) ->
  receive
    {log, From, Time, Msg} ->
      NewClock = vec:update(From, Time, Clock),
      NewQueue = orderqueue:insert({From, Time, Msg}, Time, Queue),
      RemainingQueue = log(NewClock, NewQueue),
      loop(NewClock, RemainingQueue)
  end.

log(_, []) -> [];
log(Clock, [{Time, {F, T, M}} | Rest]) ->
  case vec:safe(Time, Clock) of
    true ->
      log(F, T, M),
      log(Clock, Rest);
    false -> [{Time, {F, T, M}} | Rest]
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

