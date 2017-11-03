-module(chatRoom).
-export([start/0, connect/1]).


start() ->
	{ok, Listen} = gen_tcp:listen(6789,
					[binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
	register(master, self()),
	spawn(chatRoom, connect, [Listen]), %创建新的进程负责监听端口
	handle([]).

connect(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	master!{new, Socket},
	spawn(chatRoom, connect, [Listen]),
	recv_message(Socket).


recv_message(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			[Name,Str] = binary_to_term(Bin),
			master!{message, Name, Str},
			io:format("~p: ~p~n",[Name,Str]),
			recv_message(Socket);
		{tcp_closed, Socket} ->
			master!{quit, Socket}
	end.

% send message to all client
send_message([Socket | Set], Message) ->
	gen_tcp:send(Socket, term_to_binary(Message)),
	send_message(Set, Message);
send_message([], Message) ->
	done.

% M进程，管理所有的服务器连接进程，某一个连接进程收到客户消息，发送给M进程
% M进程再发送给所有客户，完成广播。
handle(Set) ->
	% io:format("~w: ~w~n",[self(), Set]),
	receive
		{new, Socket} ->
			io:format("~w~n", [Socket]),
			handle([Socket|Set]);
		{message, Name, Message} ->
			io:format("~p: ~p~n", [Name, Message]),
			send_message(Set, [Name,Message]),
			% SendMessage = fun(Socket) ->
			% 	gen_tcp:send(Socket, term_to_binary(Message)) end,
			% lists:foreach(SendMessage, Set),
			handle(Set);
		{quit, Socket} ->
			handle(lists:delete(Socket, Set))
	end.


