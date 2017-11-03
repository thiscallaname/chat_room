%%gen_server代码模板
-module(listen).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
-export([start/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

 start() ->
 	{ok, Listen} = gen_tcp:listen(6789, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
 	{ok, Pid} = gen_server:start_link(?MODULE, [Listen], []),
 	m:start(),
    {ok, Upid} = user_table:start_link(),
 	Pid ! {listen, Listen}.

% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([Listen]) ->
    {ok, Listen}.

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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info({listen,Listen}, State) ->
 	{ok, Socket} = gen_tcp:accept(Listen),
 	self() ! {connect, Socket},
 	self() ! {listen, Listen},
    {noreply, State};

handle_info({connect,Socket}, State) ->
 	% {ok, Pid} = gen_server:start_link(server, [Socket], []),
    {ok, Pid} = server:start(Socket),
 	gen_tcp:controlling_process(Socket, Pid),
    {noreply, State};

handle_info(Info, State) ->
	io:format("info"),
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