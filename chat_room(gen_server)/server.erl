-module(server).

%%gen_server代码模板

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
-define(SERVER, ?MODULE).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0]).

start() ->
	done.

init([Socket]) ->
    {ok, Socket}.

handle_call(Info, From, State) ->
	io:format("call:~w~n", [Info]),
    Reply = hello,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Bin}, State) ->
	[Name, Message] = binary_to_term(Bin),
	io:format("tcp:~p~p~n", [Name, Message]),
	m:cast([Name,Message]),
    {noreply, State};

handle_info({cast,Name,Message}, State) ->
	gen_tcp:send(State, term_to_binary([Name, Message])),
    {noreply, State};

handle_info({tcp_closed,Port}, State) ->
	io:format("tcp_closed~p~n",[State]),
	m:leave(self()),
    {noreply, State};

handle_info(Info, State) ->
	io:format("Info:~w~n", [Info]),
    {noreply, State}.


terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
