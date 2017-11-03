-module(simpleClient).
-export([start/1, loop/3]).

start(ClientName) ->
	{ok, Socket} = gen_tcp:connect("localhost", 6789, [binary, {packet, 4}]),
	spawn(?MODULE,loop,[Socket, ClientName, self()]),
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
			gen_tcp:send(Socket, term_to_binary([ClientName,Str])),
			loop(Socket, ClientName, Ppid)
	end.

print_message(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			[Name, Message] = binary_to_term(Bin),
			io:format("~p:  ~p~n", [Name, Message]),
			print_message(Socket);
		{quit} ->
			io:format("Bye!")
	end.	

