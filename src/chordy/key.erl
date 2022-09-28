%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2022 16:20
%%%-------------------------------------------------------------------
-module(key).
-author("frane").

%% API
-export([generate/0, between/3]).


generate() ->
	rand:uniform().

between(_Key, From, To) when From == To -> true;
between(Key, From, To) when From > To -> (Key > From) or (Key =< To);
between(Key, From, To) -> (Key > From) and (Key =< To).