%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2022 16:20
%%%-------------------------------------------------------------------
-module(node3).

-author("frane").

%% API
-export([start/1, start/2, batch/2]).

-define(Stabilize, 1000).
-define(Timeout, 10000).

batch(0, _) ->
    ok;
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
    node(Id, Predecessor, monitor(Successor), storage:create(), nil).

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

node(Id, Predecessor, Successor, Store, Next) ->
    {_Skey, Sref, Spid} = Successor,
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);
        {notify, New} ->
            {Pred, Updated} = notify(New, Id, Predecessor, Store, Successor),
            drop(Predecessor),
            node(Id, monitor(Pred), Successor, Updated, Next);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {status, Pred, Nx} ->
            {Succ, Nex} = stabilize(Pred, Id, Successor, Nx),
            drop(Successor),
            node(Id, Predecessor, monitor(Succ), Store, Nex);
        state ->
            io:format("node [~w]: store size ~w pred ~w succ ~w~n", [Id, length(Store), Predecessor, Successor]),
            node(Id, Predecessor, Successor, Store, Next);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        probe ->
            Spid ! {probe, Id, [Id], {erlang:monotonic_time(millisecond), length(Store)}},
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, {Time, Size}} ->
            Duration = erlang:monotonic_time(millisecond) - Time,
            io:format("Node [~w]: ~n\tprobe took ~w ms~n\tstore size ~w~n\tparticipating "
                      "nodes ~w~n",
                      [Id, Duration, Size, length(Nodes)]),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, I, Nodes, {Time, Size}} ->
            Spid ! {probe, I, [Id | Nodes], {Time, Size + length(Store)}},
            node(Id, Predecessor, Successor, Store, Next);
        {collect, Q, C} ->
            Spid ! {collect, Q, C, Id, Store},
            node(Id, Predecessor, Successor, Store, Next);
        {collect, Q, Client, Id, Collect} ->
            Client ! {collect, Q, Collect},
            node(Id, Predecessor, Successor, Store, Next);
        {collect, Q, C, I, Collect} ->
            Spid ! {collect, Q, C, I, storage:merge(Store, Collect)},
            node(Id, Predecessor, Successor, Store, Next);
        validate ->
            validate(Id, Predecessor, Store),
            Spid ! {validate, Id},
            node(Id, Predecessor, Successor, Store, Next);
        {validate, Id} ->
            node(Id, Predecessor, Successor, Store, Next);
        {validate, I} ->
            validate(Id, Predecessor, Store),
            Spid ! {validate, I},
            node(Id, Predecessor, Successor, Store, Next);
        {add, Key, Value, Qref, Client} ->
            Updated = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Updated, Next);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {handover, Elements} ->
            Merged = storage:merge(Elements, Store),
            node(Id, Predecessor, Successor, Merged, Next);
        {'DOWN', Ref, process, _, _ } -> 
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);
        Error ->
            io:format("node [~w]: Strange message ~w~n", [Id, Error])
    end.

down(Ref, {_, Ref, _}, Successor, Next) ->
        {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, New) ->
    {Predecessor, monitor(New), nil}.

validate(Id, {Pkey, _, _}, Store) ->
    case lists:all(fun({Key, _}) -> key:between(Key, Pkey, Id) end, Store) of
        true ->
            ok;
        false ->
            io:format("node [~w]: Invalid store~n", [Id])
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Updated = storage:add(Key, Value, Store),
            Client ! {Qref, ok},
            Updated;
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, storage:lookup(Key, Store)};
        false ->
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Handover} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Handover},
    Keep.

notify(New, Id, Predecessor, Store, {Skey, _Sref, Spid}) ->
    {Nkey, Npid} = New,
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Npid ! {status, nil, {Skey, Spid}},
            {New, Keep};
        {Pkey, _Pref, Ppid} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    Npid ! {status, New, {Skey, Spid}},
                    {New, Keep};
                false ->
                    Keep = handover(Id, Store, Pkey, Ppid),
                    Npid ! {status, {Pkey, Ppid}, {Skey, Spid}},
                    {Predecessor, Keep}
            end
    end.

request(Peer, Predecessor, {Skey, _Sref, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({Skey, Sref, Spid}) ->
    Spid ! {request, self()},
    {Skey, Sref, Spid}.

stabilize(Pred, Id, Successor, Next) ->
    {Skey, _Sref, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Id, _} ->
            {Successor, Next};
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            {Successor, Next};
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    stabilize(Pred, Id, {Xkey, Xpid}, Successor);
                false ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Next}
            end
    end.

monitor(nil) ->
    nil;
monitor({Key, Ref, Pid}) ->
    monitor(drop({Key, Ref, Pid}));
monitor({Key, Pid}) ->
    {Key, erlang:monitor(process, Pid), Pid}.
    
drop(nil) ->
    nil;
drop({Key, nil, Pid}) ->
    {Key, Pid};
drop({Key, Ref, Pid}) ->
    erlang:demonitor(Ref, [flush]),
    {Key, Pid}.
