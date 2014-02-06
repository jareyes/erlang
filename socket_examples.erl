-module(socket_examples).
-export([nano_get_url/0, start_nano_server/0, error_test/0]).
-import(lists, [reverse/1]).

nano_get_url() ->
	nano_get_url("www.google.com").

nano_get_url(Host) ->
	{ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
	ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
	receive_data(Socket, []).

receive_data(Socket, SoFar) ->
	receive
		{tcp, Socket, Bin} ->
			receive_data(Socket, [Bin|SoFar]);
		{tcp_closed, Socket} ->
			list_to_binary(reverse(SoFar))
	end.

start_nano_server() ->
	{ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
										 {reuseaddr, true},
										 {active, true}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	gen_tcp:close(Listen),
	loop(Socket).

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("Server received binary = ~p~n", [Bin]),
			Str = binary_to_term(Bin),
			Reply = lib_misc:string2value(Str),
			io:format("Server replying = ~p~n", [Reply]),
			gen_tcp:send(Socket, term_to_binary(Reply)),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Server socket closed")
	end.

error_test() ->
	spawn(fun() -> error_test_server() end),
	lib_misc:sleep(2000),
	{ok, Socket} = gen_tcp:connect("localhost", 4321, [binary, {packet, 2}]),
	io:format("connected to: ~p~n", [Socket]),
	gen_tcp:send(Socket, <<"123">>),
	receive
		Any -> io:format("Any = ~p~n", [Any])
	end.

error_test_server() ->
	{ok, Listen} = gen_tcp:listen(4321, [binary, {packet, 2}]),
	{ok, Socket} = gen_tcp:accept(Listen),
	error_test_server_loop(Socket).

error_test_server_loop(Socket) ->
	receive
		{tcp, Socket, Data} ->
			io:format("received: ~p~n", [Data]),
			_ = atom_to_list(Data),
			error_test_server_loop(Socket)
	end.