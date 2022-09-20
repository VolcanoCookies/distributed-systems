%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 15:03
%%%-------------------------------------------------------------------
-module(test).
-author("frane").

%% API
-export([run/2]).

run(Sleep, Jitter) ->
  Log = loggy:start([john, paul, ringo, george]),
  A = worker:start(john, Log, 13, Sleep, Jitter),
  B = worker:start(paul, Log, 23, Sleep, Jitter),
  C = worker:start(ringo, Log, 36, Sleep, Jitter),
  D = worker:start(george, Log, 49, Sleep, Jitter),
  E = worker:start(martin, Log, 68, Sleep, Jitter),
  F = worker:start(steven, Log, 91, Sleep, Jitter),
  worker:peers(A, [B, C, D]),
  worker:peers(B, [A, C, D]),
  worker:peers(C, [A, B, D]),
  worker:peers(D, [A, B, C]),
  timer:sleep(2500),
  worker:peers(E, [A, B, C, D, F]),
  worker:peers(F, [A, B, C, D, E]),
  timer:sleep(3500),
  loggy:stop(Log),
  worker:stop(A),
  worker:stop(B),
  worker:stop(C),
  worker:stop(D),
  worker:stop(E),
  worker:stop(F).