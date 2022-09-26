-module(gms1).

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
            slave(Id, Master, Leader, Slaves, Group)
    end.

init(Id, Master) ->
    leader(Id, Master, [], [Master]).

bcast(_, _, []) ->
    ok;
bcast(Id, Msg, [Node | Rest]) ->
    Node ! Msg,
    bcast(Id, Msg, Rest).

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
            slave(Id, Master, Leader, SlavesNew, GroupNew)
    end.
