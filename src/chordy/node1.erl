%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2022 16:20
%%%-------------------------------------------------------------------
-module(node1).
-author("frane").

%% API
-export([start/1, start/2, batch/3]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

batch(_, 0, _) -> ok;
batch(S, N, Peer) ->
	start(S, Peer),
	batch(S + 1, N - 1, Peer).

start(Id) ->
	start(Id, nil).
start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
	{_Skey, Spid} = Successor,
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		{request, Peer} ->
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} ->
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		state ->
			io:format("node [~w]: pred ~w succ ~w~n", [Id, Predecessor, Successor]),
			node(Id, Predecessor, Successor);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
		probe ->
			Spid ! {probe, Id, [Id], erlang:monotonic_time(millisecond)},
			node(Id, Predecessor, Successor);
		{probe, Id, Nodes, Time} ->
			Duration = erlang:monotonic_time(millisecond) - Time,
			io:format("Node [~w]: probe took ~w ms participating nodes ~w~n", [Id, Duration, Nodes]),
			node(Id, Predecessor, Successor);
		{probe, I, Nodes, Time} ->
			Spid ! {probe, I, [Id | Nodes], Time},
			node(Id, Predecessor, Successor);
		Error ->
			io:format("node [~w]: Strange message ~w~n", [Id, Error])
	end.

notify(New, Id, Predecessor) ->
	{Nkey, Npid} = New,
	case Predecessor of
		nil ->
			Npid ! {status, Predecessor},
			New;
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Npid ! {status, New},
					New;
				false ->
					Npid ! {status, Predecessor},
					Predecessor
			end
	end.

request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
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