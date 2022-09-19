%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 19:34
%%%-------------------------------------------------------------------
-module(intf).
-author("frane").

%% API
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
  [].

add(Name, Ref, Pid, Intf) ->
  [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, _, Pid} -> {ok, Pid};
    false -> notfound
  end.

ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, Ref, _} -> {ok, Ref};
    false -> notfound
  end.

name(Ref, Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    {Name, _, _} -> {ok, Name};
    false -> notfound
  end.

list(Intf) ->
  lists:map(fun({Name, _, _}) -> Name end, Intf).

broadcast(Message, Intf) ->
  lists:foreach(fun({_, _, Pid}) -> Pid ! Message end, Intf).

