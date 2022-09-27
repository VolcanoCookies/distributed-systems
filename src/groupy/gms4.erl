-module(gms4).

-compile(export_all).

-define(lost_message, 200).
-define(crash_on_broadcast, 500).
-define(message_timeout, 250).

start(Id) ->
	Self = self(),
	{ok, spawn_link(fun() -> init(Id, Self) end)}.

start(Id, Grp) ->
	Self = self(),
	{ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, N, [Leader | Slaves], Group} ->
			Leader ! {ack, N},
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N + 1, none, Slaves, Group)
	after 500 ->
		Master ! {error, "no reply from leader"}
	end.

init(Id, Master) ->
	leader(Id, Master, 0, [], [Master]).

ftbcast(Id, N, Msg, Nodes) ->
	Nr = length(Nodes),
	bcast(Id, Msg, Nodes),
	case await(Id, N, Nr) of
		repeat ->
			ftbcast(Id, N, Msg, Nodes);
		_ ->
			ok
	end.

bcast(_, _, []) ->
	ok;
bcast(Id, Msg, [Node | Rest]) ->
	crash(Id),
	case rand:uniform(?lost_message) of
		1 ->
			io:format("Oopsie, dropped a message...~n");
		_ ->
			Node ! Msg
	end,
	bcast(Id, Msg, Rest).

crash(Id) ->
	case rand:uniform(?crash_on_broadcast) of
		1 ->
			io:format("leader [~w]: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.

await(_, _, 0) ->
	ok;
await(Id, N, Nr) ->
	receive
		{ack, I} when I < N ->
			await(Id, N, Nr);
		{ack, N} ->
			await(Id, N, Nr - 1)
	after ?message_timeout ->
		repeat
	end.


% The leader process,

leader(Id, Master, N, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			ftbcast(Id, N, {msg, N, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, N + 1, Slaves, Group);
		{join, Wrk, Peer} ->
			SlavesNew = lists:append(Slaves, [Peer]),
			GroupNew = lists:append(Group, [Wrk]),
			ftbcast(Id, N, {view, N, [self() | SlavesNew], GroupNew}, SlavesNew),
			Master ! {view, GroupNew},
			leader(Id, Master, N + 1, SlavesNew, GroupNew);
		stop ->
			ok
	end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
	receive
		{msg, I, _} when I < N ->
			Leader ! {ack, I},
			io:format("slave [~w]: Ignoring msg order ~w~n", [Id, I]),
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{view, I, _, _} when I < N ->
			Leader ! {ack, I},
			io:format("slave [~w]: Ignoring view order ~w~n", [Id, I]),
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, N, Msg} ->
			Leader ! {ack, N},
			Master ! Msg,
			slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group);
		{view, N, [Leader | SlavesNew], GroupNew} ->
			Leader ! {ack, N},
			Master ! {view, GroupNew},
			slave(Id, Master, Leader, N + 1, Last, SlavesNew, GroupNew);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, N, Last, Slaves, Group);
		Error ->
			io:format("slave [~w]: order ~w strange message: ~w~n", [Id, N, Error])
	end.

election(Id, Master, N, Last, Slaves, [_ | Group]) ->
	Self = self(),
	case Slaves of
		[Self | Rest] ->
			ftbcast(Id, N - 1, Last, Rest),
			ftbcast(Id, N, {view, N, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master, N + 1, Rest, Group);
		[Leader | Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest, Group)
	end.