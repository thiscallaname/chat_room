-module(roomContr).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
 
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
 
-export([start/0, join/2, list/0,find/1,delete/1]).
 
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
 
start() ->
    io:format("room control: ~p~n", [self()]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
%% @doc  
join(RoomName, RoomPid) ->
    io:format("room control join~n"),
    gen_server:call(?SERVER, {join, RoomPid, RoomName}).

find(RoomName) ->
    {find,RoomPid} = gen_server:call(?SERVER, {find, RoomName}),
    RoomPid.

delete(RoomName) ->
    gen_server:call(?SERVER, {delete, RoomName}).

list() ->
    gen_server:call(?SERVER, {list}).
 
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
 
init(Args) ->
    {ok, dict:new()}.

handle_call({join, RoomPid, RoomName}, _From, State) ->
    {reply, ok, dict:store(RoomName,RoomPid, State)};

handle_call({list}, _From, State) ->
    L = dict:to_list(State),
    % listAll(L),
    {reply, {ok, L}, State};

handle_call({find, RoomName}, _From, State) ->
    {ok, RoomPid} = dict:find(RoomName,State),
    {reply, {find,RoomPid}, State};

handle_call({delete, RoomName}, _From, State) ->
    io:format("no one in ~p, chatroom is deleted~n", [RoomName]),
    NewState = dict:erase(RoomName, State),
    {reply, {ok}, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

listAll([{Name, Pid}|T]) -> 
    io:format("~p ---->~p~n", [Name, Pid]),
    listAll(T);

listAll([]) -> ok.
