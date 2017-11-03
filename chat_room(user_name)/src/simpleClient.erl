-module(simpleClient).
-export([sign/1, start/1, loop/3]).

sign(NickName) ->
	{ok, Socket} = gen_tcp:connect("localhost", 6789, [binary, {packet, 4}]),
	gen_tcp:send(Socket, term_to_binary([sign,NickName])),%,
	receive
		{tcp, Socket, Bin} ->
			[index, Index] = binary_to_term(Bin),
			io:format("~p~n", [Index])
			% print_message(Socket)
	end.

start(UserId) ->
	{ok, Socket} = gen_tcp:connect("localhost", 6789, [binary, {packet, 4}]),
	gen_tcp:send(Socket, term_to_binary([first,UserId])),
	receive
		{tcp, Socket, Bin} ->
			Message = binary_to_term(Bin),
			case Message of 
				empty ->
					io:format("Your ID is wrong~n"),
					gen_tcp:close(Socket),
					ClientName = "empty",
					exit(normal);
				_ ->
					io:format("~p~n", [Message]),
					ClientName = Message
			end;
		_ ->
			ClientName = "empty"	
	end,
	if 
		ClientName =/= "empty" ->
			spawn(?MODULE,loop,[Socket, ClientName, self()]);
		true ->
			done
	end,
	print_message(Socket).

loop(Socket, ClientName, Ppid) ->
	Line = io:get_line("<<"),
	Str = lists:delete(10, Line), %remove "\n" from the line.
	% io:format("~p~n",[Str]),
	if 
		Str =:= "q" ->
			gen_tcp:close(Socket),
			Ppid ! {quit};
		true ->
			gen_tcp:send(Socket, term_to_binary([message,Str])),
			loop(Socket, ClientName, Ppid)
	end.

print_message(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			Message = binary_to_term(Bin),
			io:format("~p~n", [Message]),
			print_message(Socket);
		{quit} ->
			io:format("Bye!")
	end.	

