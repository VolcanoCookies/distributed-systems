-module(gms3).

-compile(export_all).

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
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N + 1, none, Slaves, Group)
	after 500 ->
		Master ! {error, "no reply from leader"}
	end.

init(Id, Master) ->
	leader(Id, Master, 0, [], [Master]).

bcast(_, _, []) ->
	ok;
bcast(Id, Msg, [Node | Rest]) ->
	Node ! Msg,
	crash(Id),
	bcast(Id, Msg, Rest).

crash(Id) ->
	case rand:uniform(500) of
		250 ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.

% The leader process,

leader(Id, Master, N, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, N, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, N + 1, Slaves, Group);
		{join, Wrk, Peer} ->
			SlavesNew = lists:append(Slaves, [Peer]),
			GroupNew = lists:append(Group, [Wrk]),
			bcast(Id, {view, N, [self() | SlavesNew], GroupNew}, SlavesNew),
			Master ! {view, GroupNew},
			leader(Id, Master, N + 1, SlavesNew, GroupNew);
		stop ->
			ok
	end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
	receive
		{msg, I, _} when I < N ->
			io:format("slave: Ignoring msg order ~w~n", [I]),
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{view, I, _, _} when I < N ->
			io:format("slave: Ignoring view order ~w~n", [I]),
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, N, Msg} ->
			Master ! Msg,
			slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group);
		{view, N, [Leader | SlavesNew], GroupNew} ->
			Master ! {view, GroupNew},
			slave(Id, Master, Leader, N + 1, Last, SlavesNew, GroupNew);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, N, Last, Slaves, Group);
		Error ->
			io:format("slave: order ~w strange message: ~w~n", [N, Error])
	end.

election(Id, Master, N, Last, Slaves, [_ | Group]) ->
	Self = self(),
	case Slaves of
		[Self | Rest] ->
			bcast(Id, Last, Rest),
			bcast(Id, {view, N, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master, N + 1, Rest, Group);
		[Leader | Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest, Group)
	end.