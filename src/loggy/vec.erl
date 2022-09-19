%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 20:00
%%%-------------------------------------------------------------------
-module(vec).
-author("frane").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).


zero() ->
  [].

inc(Name, Time) ->
  case lists:keytake(Name, 1, Time) of
    {value, {Name, N}, Rest} -> [{Name, N + 1} | Rest];
    false -> [{Name, 1} | Time]
  end.

merge([], _) -> [];
merge([{Name, Ti} | Rest], Time) ->
  case lists:keytake(Name, 1, Time) of
    {value, {Name, N}, Rem} when Ti > N -> [{Name, Ti} | merge(Rest, Rem)];
    {value, {Name, N}, Rem} -> [{Name, N} | merge(Rest, Rem)];
    false -> [{Name, Ti} | merge(Rest, Time)]
  end.

leq([], _) -> true;
leq([{Name, Ti} | Rest], Time) ->
  case lists:keytake(Name, 1, Time) of
    {value, {Name, N}, _} when Ti > N -> false;
    {value, {Name, _}, _} -> leq(Rest, Time);
    false -> false
  end.

clock(_) ->
  [].

update(From, Time, Clock) ->
  {From, Ta} = lists:keyfind(From, 1, Time),
  case lists:keyfind(From, 1, Clock) of
    {From, Tb} when Ta > Tb -> lists:keyreplace(From, 1, Clock, {From, Ta});
    {From, Tb} -> lists:keyreplace(From, 1, Clock, {From, Tb});
    false ->
      [{From, Ta} | Clock]
  end.

safe(Time, Clock) ->
  leq(Time, Clock).