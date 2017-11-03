-module(server).
%%gen_server
-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
-define(SERVER, ?MODULE).
% -record(state, {}).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1]).
-record(contain, {socket, username}). 
% start() ->
% 	{ok, Listen} = gen_tcp:listen(6789, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
%     m:start(),
% 	spawn(fun() -> wait(Listen) end).
start(Socket) ->
	{ok, Pid} = gen_server:start_link(?MODULE, [Socket], []),
	{ok, Pid}.
% wait(Listen) ->
% 	{ok, Socket} = gen_tcp:accept(Listen),
% 	{ok, Pid} = gen_server:start_link(?MODULE, [Socket], []),
% 	m:join(Pid),
% 	gen_tcp:controlling_process(Socket, Pid),
% 	wait(Listen).

% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([Socket]) ->
	% io:format("~p~n",[self()]),
	NewBind = #contain{socket = Socket},
    {ok, NewBind}.

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
% handle_call({recv}, From, State) ->
% 	io:format("recving~n"),
% 	% m:recv([Name,Str]),
%     Reply = ok,
%     {reply, Reply, State};


handle_call(Info, From, State) ->
	io:format("call:~w~n", [Info]),
    Reply = hello,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	% io:format("cast~w~n",[Msg]),
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------

handle_info({tcp, Socket, Bin}, State) ->
	Port = State#contain.socket,
	case binary_to_term(Bin) of 
		[first, UserId] ->
			case user_table:lookup(UserId) of 
				{ok, Name} -> 
					gen_tcp:send(Port, term_to_binary(Name)),
					m:join(Name, self()),
					NewState = State#contain{username = Name},
					{noreply, NewState};
				_ ->
					gen_tcp:send(Port, term_to_binary(empty)),
					gen_tcp:close(Port),
					m:leave(self()),
					{noreply, State}
			end;					
			% gen_server:call(user_table, {lookup, UserId});
		[sign, UserName] ->
			{index, Index} = user_table:add(UserName),
			gen_tcp:send(Port, term_to_binary([index, Index])),
			{noreply, State};
		[message, Message] ->
			Name = State#contain.username,
			io:format("~p: ~p~n", [Name, Message]),
			m:cast([Name,Message]),
			{noreply, State}
	end;
    % {noreply, State};

handle_info({cast,Name,Message}, State) ->
	% io:format("send_back:~p~p~p~n", [Name, Message,State]),
	Port = State#contain.socket,
	gen_tcp:send(Port, term_to_binary([Name, Message])),
    {noreply, State};

handle_info({tcp_closed,Port}, State) ->
	% io:format("tcp_closed~p~n",[self()]),
	m:leave(self()),
    {noreply, State};

% handle_info({name, Name}, State) ->
	
% 	io:format("Info:~w~n", [Name]),
% 	gen_tcp:send(State, term_to_binary(Name)),
%     {noreply, State};

% handle_info({index, Index}, State) ->
% 	io:format("Info:~w~n", [Index]),
% 	gen_tcp:send(State, term_to_binary([index, Index])),
%     {noreply, State};

% handle_info({empty}, State) ->
% 	gen_tcp:send(State, term_to_binary([empty])),
%     {noreply, State};

handle_info(Info, State) ->
	io:format("Info:~w~n", [Info]),
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.
