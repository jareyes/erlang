-module(lib_misc).
-export([pythag/1, perms/1, tuple_to_list/1, fac/1, on_exit/2, keep_alive/2, 
	sleep/1, file_size_and_type/1, ls/1]).
-include_lib("kernel/include/file.hrl").

keep_alive(Name, Fun) ->
	register(Name, Pid = spawn(Fun)),
	on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).

on_exit(Pid, Fun) ->
	spawn(
		fun() ->
			Ref = monitor(process, Pid),
			receive {'DOWN', Ref, process, Pid, Why} ->
				Fun(Why)
			end
		end
	).

pythag(N) -> [ {A,B,C} || 
	A <- lists:seq(1,N),
	B <- lists:seq(1,N),
	C <- lists:seq(1,N),
	A+B+C =< N,
	A*A+B*B =:= C*C
].

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

tuple_to_list(T) when tuple_size(T) =:= 0-> [];
tuple_to_list(T) -> tuple_to_list(T, [], 1).

tuple_to_list(T, L, N) when N > tuple_size(T) -> lists:reverse(L);
tuple_to_list(T, L, N) -> tuple_to_list(T, [element(N, T)|L], N+1).

fac(N) when N < 0 -> exit(negativeInput);
fac(0) -> 1;
fac(N) -> fac(N, 1).

fac(1, Acc) -> Acc;
fac(N, Acc) -> fac(N-1, Acc*N).

sleep(Time) ->
	receive
	after Time ->
		true
	end.

file_size_and_type(File) ->
	case file:read_file_info(File) of
		{ok, Facts} ->
			{Facts#file_info.type, Facts#file_info.size};
			% Name suggests that type and size probably should be in the other order
		_ -> 
			error
	end.

ls(Dir) ->
	{ok, L} = file:list_dir(Dir),
	lists:map(fun(Item) -> {Item, file_size_and_type(Item)} end, lists:sort(L)).