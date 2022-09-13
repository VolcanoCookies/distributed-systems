%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2022 15:30
%%%-------------------------------------------------------------------
-module(ping).
-author("frane").

%% API
-export([ping/0, pong/0]).

ping() ->
  %register(ping, self()),
  {pong, 'pong@DESKTOP-LC12GU3'} ! echo.

pong() ->
  register(pong, self()),
  receive
    echo -> {ping, 'ping@DESKTOP-LC12GU3'} ! {pong}
  end.