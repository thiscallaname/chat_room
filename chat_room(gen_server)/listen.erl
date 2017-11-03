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
 	Pid ! {listen, Listen}.

%------------------------
init([Listen]) ->
    {ok, Listen}.

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info({listen,Listen}, State) ->
 	{ok, Socket} = gen_tcp:accept(Listen),
 	self() ! {connect, Socket},
 	self() ! {listen, Listen},
    {noreply, State};

handle_info({connect,Socket}, State) ->
 	{ok, Pid} = gen_server:start_link(server, [Socket], []),
 	m:join(Pid),
 	gen_tcp:controlling_process(Socket, Pid),
    {noreply, State};

handle_info(Info, State) ->
	io:format("info"),
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
