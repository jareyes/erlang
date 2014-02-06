-module(broadcast).
-compile(export_all).

send(IoList) ->
	case inet:ifget("eth0", [broadaddr]) of
		{ok, [{broadaddr, Ip}]} ->
			{ok, Socket} = gen_udp:open(5010, [{brodcast, true}]),
			gen_udp:send(Socket, Ip, 6000, IoList),
			gen_udp:close(Socket);
		_ ->
			io:format("Bad interface name, or broadcasting not supported~n")
	end.

listen() ->
	{ok, _} = gen_udp:open(6000),
	loop().

loop() ->
	receive
		Any ->
			io:format("received: ~p~n", [Any])
	end.