%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 19:47
%%%-------------------------------------------------------------------
-module(hist).
-author("frane").

%% API
-export([new/1, update/3]).


new(Name) ->
  [{Name, inf}].

update(Node, N, History) ->
  case lists:keytake(Node, 1, History) of
    {value, {Node, M}, _} when M >= N -> old;
    {value, {Node, _}, Rest} -> {new, [{Node, N} | Rest]};
    false -> {new, [{Node, N} | History]}
  end.