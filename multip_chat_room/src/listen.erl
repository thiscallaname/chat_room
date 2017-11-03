-module(listen).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
 
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
 
-export([start/0]).
 
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
 
start() ->
    {ok, Listen} = gen_tcp:listen(6789, [binary, {packet, 4},{reuseaddr, true}, {active, true}]),  
    {ok, Pid} = gen_server:start_link(?MODULE, [Listen], []),
    roomContr:start(),
    user_table:start(),
    Pid ! {listen, Listen}.

 
%% @doc  just a test
% join() ->
%     gen_server:call(?MODULE, )

 
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
 
init([Listen]) ->
    {ok, Listen}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc listen 
handle_info({listen, Listen}, State) ->
    io:format("listen : ~p~n",[Listen]),
    {ok, Socket} = gen_tcp:accept(Listen),
    self() ! {connect, Socket},
    self() ! {listen, Listen},
    {noreply, State};

%% @doc start a new server to connect 
handle_info({connect, Socket},  State) ->
    io:format("connect~p~n",[Socket]),
    {ok,Pid} = server:start(Socket),
    gen_tcp:controlling_process(Socket, Pid),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------