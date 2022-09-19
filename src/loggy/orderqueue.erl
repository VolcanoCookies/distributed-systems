%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 15:52
%%%-------------------------------------------------------------------
-module(orderqueue).
-author("frane").

%% API
-export([new/0, insert/3]).


new() -> [].

insert(Elem, Time, []) -> [{Time, Elem}];
insert(Elem, Time, [{T, E} | Rest]) ->
  case vec:leq(Time, T) of
    true -> [{Time, Elem}, {T, E} | Rest];
    false -> [{T, E} | insert(Elem, Time, Rest)]
  end.