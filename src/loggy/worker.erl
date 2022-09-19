%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 14:54
%%%-------------------------------------------------------------------
-module(worker).
-author("frane").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, vec:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Clock) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      NewTime = vec:inc(Name, vec:merge(Time, Clock)),
      Log ! {log, Name, NewTime, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, NewTime);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, Clock, {error, Error}}
  after Wait ->
    NewClock = vec:inc(Name, Clock),
    Selected = select(Peers),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, NewClock, Message},
    jitter(Jitter),
    Log ! {log, Name, NewClock, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, NewClock)
  end.

loop_old(Name, Log, Peers, Sleep, Jitter) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      Log ! {log, Name, Time, {received, Msg}},
      loop_old(Name, Log, Peers, Sleep, Jitter);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
    Selected = select(Peers),
    Time = na,
    Message = {hello, random:uniform(100)},
    Selected ! {msg, Time, Message},
    jitter(Jitter),
    Log ! {log, Name, Time, {sending, Message}},
    loop_old(Name, Log, Peers, Sleep, Jitter)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).

