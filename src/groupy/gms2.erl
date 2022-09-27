-module(gms2).

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
		{view, [Leader | Slaves], Group} ->
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Slaves, Group)
	after 500 ->
		Master ! {error, "no reply from leader"}
	end.

init(Id, Master) ->
	random:seed(random:uniform(100), random:uniform(100), random:uniform(100)),
	leader(Id, Master, [], [Master]).

bcast(_, _, []) ->
	ok;
bcast(Id, Msg, [Node | Rest]) ->
	Node ! Msg,
	crash(Id),
	bcast(Id, Msg, Rest).

crash(Id) ->
	case random:uniform(100) of
		100 ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.

% The leader process,

leader(Id, Master, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, Slaves, Group);
		{join, Wrk, Peer} ->
			SlavesNew = lists:append(Slaves, [Peer]),
			GroupNew = lists:append(Group, [Wrk]),
			bcast(Id, {view, [self() | SlavesNew], GroupNew}, SlavesNew),
			Master ! {view, GroupNew},
			leader(Id, Master, SlavesNew, GroupNew);
		stop ->
			ok
	end.

slave(Id, Master, Leader, Slaves, Group) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Slaves, Group);
		{join, Wrk, Peer} ->
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, Slaves, Group);
		{msg, Msg} ->
			Master ! Msg,
			slave(Id, Master, Leader, Slaves, Group);
		{view, [Leader | SlavesNew], GroupNew} ->
			Master ! {view, GroupNew},
			slave(Id, Master, Leader, SlavesNew, GroupNew);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, Slaves, Group)
	end.

election(Id, Master, Slaves, [_ | Group]) ->
	Self = self(),
	case Slaves of
		[Self | Rest] ->
			bcast(Id, {view, Slaves, Group}, Rest),
			Master ! {view, Group},
			leader(Id, Master, Rest, Group);
		[Leader | Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Rest, Group)
	end.