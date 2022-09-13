%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2022 22:10
%%%-------------------------------------------------------------------
-module(hello).
-author("frane").

%% API
-export([world/0, hello/0, first/0, second/0, last/0]).

world()->
  "Hello world!".

hello() ->
  receive
    X -> io:format("aaa! surprise, a message: ~s~n", [X])
  end.

first() ->
  receive
    {tic, X}->
      io:format("tic: ~w~n", [X]),
      second()
  end.

second() ->
  receive
    {tac, X} ->
      io:format("tac: ~w~n", [X]),
      last();
    {toe, X} ->
      io:format("toe: ~w~n", [X]),
      last()
  end.

last() ->
  receive
    X ->
      io:format("end: ~w~n", [X])
  end.