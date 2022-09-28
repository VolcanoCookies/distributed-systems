%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2022 16:20
%%%-------------------------------------------------------------------
-module(node2).
-author("frane").

%% API
-export([start/1, start/2, batch/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

batch(0, _) -> ok;
batch(N, Peer) ->
	start(key:generate(), Peer),
	batch(N - 1, Peer).

start(Id) ->
	start(Id, nil).
start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
	{ok, {Id, self()}};
connect(Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey, Peer}}
	after ?Timeout ->
		io:format("Time out: no response")
	end.

node(Id, Predecessor, Successor, Store) ->
	{_Skey, Spid} = Successor,
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store);

		{notify, New} ->
			{Pred, Updated} = notify(New, Id, Predecessor, Store),
			node(Id, Pred, Successor, Updated);

		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor, Store);

		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ, Store);

		state ->
			io:format("node [~w]: store size ~w pred ~w succ ~w~n", [Id, length(Store), Predecessor, Successor]),
			node(Id, Predecessor, Successor, Store);

		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Store);

		probe ->
			Spid ! {probe, Id, [Id], {erlang:monotonic_time(millisecond), length(Store)}},
			node(Id, Predecessor, Successor, Store);

		{probe, Id, Nodes, {Time, Size}} ->
			Duration = erlang:monotonic_time(millisecond) - Time,
			io:format("Node [~w]: ~n\tprobe took ~w ms~n\tstore size ~w~n\tparticipating nodes ~w~n", [Id, Duration, Size, length(Nodes)]),
			node(Id, Predecessor, Successor, Store);

		{probe, I, Nodes, {Time, Size}} ->
			Spid ! {probe, I, [Id | Nodes], {Time, Size + length(Store)}},
			node(Id, Predecessor, Successor, Store);

		{collect, Q, C} ->
			Spid ! {collect, Q, C, Id, Store},
			node(Id, Predecessor, Successor, Store);

		{collect, Q, Client, Id, Collect} ->
			Client ! {collect, Q, Collect},
			node(Id, Predecessor, Successor, Store);

		{collect, Q, C, I, Collect} ->
			Spid ! {collect, Q, C, I, storage:merge(Store, Collect)},
			node(Id, Predecessor, Successor, Store);

		validate ->
			validate(Id, Predecessor, Store),
			Spid ! {validate, Id},
			node(Id, Predecessor, Successor, Store);

		{validate, Id} ->
			node(Id, Predecessor, Successor, Store);

		{validate, I} ->
			validate(Id, Predecessor, Store),
			Spid ! {validate, I},
			node(Id, Predecessor, Successor, Store);

		{add, Key, Value, Qref, Client} ->
			Updated = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Updated);

		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store);

		{handover, Elements} ->
			Merged = storage:merge(Elements, Store),
			node(Id, Predecessor, Successor, Merged);

		Error ->
			io:format("node [~w]: Strange message ~w~n", [Id, Error])
	end.

validate(Id, {Pkey, _}, Store) ->
	case lists:all(fun({Key, _}) -> key:between(Key, Pkey, Id) end, Store) of
		true -> ok;
		false ->
			io:format("node [~w]: Invalid store~n", [Id])
	end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Updated = storage:add(Key, Value, Store),
			Client ! {Qref, ok},
			Updated;
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true -> Client ! {Qref, storage:lookup(Key, Store)};
		false -> Spid ! {lookup, Key, Qref, Client}
	end.

handover(Id, Store, Nkey, Npid) ->
	{Keep, Handover} = storage:split(Nkey, Id, Store),
	Npid ! {handover, Handover},
	Keep.

notify(New, Id, Predecessor, Store) ->
	{Nkey, Npid} = New,
	case Predecessor of
		nil ->
			Keep = handover(Id, Store, Nkey, Npid),
			Npid ! {status, Predecessor},
			{New, Keep};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Id, Store, Nkey, Npid),
					Npid ! {status, New},
					{New, Keep};
				false ->
					Npid ! {status, Predecessor},
					{Predecessor, Store}
			end
	end.

request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{_Pkey, _Ppid} ->
			Peer ! {status, Predecessor}
	end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_Skey, Spid}) ->
	Spid ! {request, self()},
	{_Skey, Spid}.
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} ->
			Successor;
		{Skey, _} ->
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
					stabilize(Pred, Id, {Xkey, Xpid});
				false ->
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.