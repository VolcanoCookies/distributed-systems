%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2022 11:32
%%%-------------------------------------------------------------------
-module(storage).
-author("frane").

%% API
-export([create/0, add/3, lookup/2, split/3, merge/2]).


create() ->
	[].

add(Key, Value, Store) ->
	lists:keystore(Key, 1, Store, {Key, Value}).

lookup(Key, Store) ->
	lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
	lists:foldl(fun({Key, Value}, {Updated, Rest}) ->
		case key:between(Key, From, To) of
			true -> {[{Key, Value} | Updated], Rest};
			false -> {Updated, [{Key, Value} | Rest]}
		end end, {[], []}, Store).

merge([], Store) -> Store;
merge([{Key, Value} | Rest], Store) ->
	merge(Rest, add(Key, Value, Store)).