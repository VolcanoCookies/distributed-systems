%%%-------------------------------------------------------------------
%%% @author frane
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2022 18:43
%%%-------------------------------------------------------------------
-module(server_improved).
-author("frane").

%% API
-export([start/2, stop/0]).

start(Port, Workers) ->
  register(rudy_parallel, spawn(fun() -> init(Port, Workers) end)).

stop() ->
  exit(whereis(rudy_parallel), "time to die").

init(Port, Workers) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Socket} ->
      io:format("Listening on port ~p with ~p workers~n", [Port, Workers]),
      worker(Socket, Workers),
      gen_tcp:close(Socket);
    {error, Error} ->
      io:format("Error: ~p~n", [Error])
  end.

worker(Socket, 1) ->
  handler(Socket);
worker(Socket, N) ->
  spawn_link(fun() -> handler(Socket) end),
  worker(Socket, N - 1).

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client);
    {error, Error} ->
      error
  end,
  handler(Listen).

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

reply({{get, URI, _}, _, Body}) ->
  timer:sleep(40),
  http:ok(Body).