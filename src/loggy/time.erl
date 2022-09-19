%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 15:26
%%%-------------------------------------------------------------------
-module(time).
-author("frane").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).


zero() ->
  0.

inc(Name, T) ->
  T + 1.

merge(Ta, Tb) when Ta > Tb ->
  Ta;
merge(_, Tb) ->
  Tb.

leq(Ta, Tb) ->
  Tb >= Ta.

clock([]) -> [];
clock([Node | Rest]) ->
  [{Node, zero()} | clock(Rest)].

update(Node, _, []) ->
  [clock([Node])];
update(Node, Time, [{Node, T} | Rest]) ->
  [{Node, merge(Time, T)} | Rest];
update(Node, Time, [H | Rest]) ->
  [H | update(Node, Time, Rest)].

safe(_, []) -> true;
safe(Time, [{_, T} | Rest]) ->
  leq(Time, T) and safe(Time, Rest).