-module(simpleClient).
-export([start/1, loop/3]).

start(ClientName) ->
	{ok, Socket} = gen_tcp:connect("localhost", 6789, [binary, {packet, 4}]),
	spawn(?MODULE,loop,[Socket, ClientName, self()]), %开启loop新进程
	print_message(Socket).

loop(Socket, ClientName, Ppid) -> %尾递归从控制台接收信息发送给服务器
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

print_message(Socket) -> %打印从服务器接收到的信息
	receive
		{tcp, Socket, Bin} ->
			[Name, Message] = binary_to_term(Bin),
			io:format("~p:  ~p~n", [Name, Message]),
			print_message(Socket);
		{quit} ->
			io:format("Bye!")
	end.

