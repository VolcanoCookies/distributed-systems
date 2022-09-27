-module(test).

-compile(export_all).


% Used to create the first worker, try:
%
% W1 = test:first(1, gms1, 1000)

run(Module) ->
	W1 = first(1, Module, 1000),
	add(2, Module, W1, 1000),
	add(3, Module, W1, 1000),
	add(4, Module, W1, 1000),
	add(5, Module, W1, 1000),
	add(6, Module, W1, 1000),
	add(7, Module, W1, 1000),
	add(8, Module, W1, 1000),
	add(9, Module, W1, 1000).

late(Module) ->
	W1 = first(1, Module, 1000),
	add(2, Module, W1, 1000),
	add(3, Module, W1, 1000),
	add(4, Module, W1, 1000),
	add(5, Module, W1, 1000),
	add(6, Module, W1, 1000),
	add(7, Module, W1, 1000),
	add(8, Module, W1, 1000),
	add(9, Module, W1, 1000).

first(N, Module, Sleep) ->
	worker:start(N, Module, rand:uniform(256), Sleep).

% Used to create additional workers, try:
%
%  test:add(2, gms1, W1, 1000) and 
%  test:add(3, gms1, W1, 1000) and ...

add(N, Module, Wrk, Sleep) ->
	worker:start(N, Module, rand:uniform(256), Wrk, Sleep).

%% To create a number of workers in one go, 

more(N, Module, Sleep) when N > 1 ->
	Wrk = first(1, Module, Sleep),
	Ns = lists:seq(2, N),
	lists:map(fun(Id) -> add(Id, Module, Wrk, Sleep) end, Ns),
	Wrk.


% These are messages that we can send to one of the workers. It will
% multicast it to all workers. They should (if everything works)
% receive the message at the same (logical) time.

freeze(Wrk) ->
	Wrk ! {send, freeze}.

go(Wrk) ->
	Wrk ! {send, go}.

sleep(Wrk, Sleep) ->
	Wrk ! {send, {sleep, Sleep}}.

			  

















