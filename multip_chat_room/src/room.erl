-module(room).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
 
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
 
-export([create_room/1, join/2, leave/3, send/3, broadcast/3, hello/1]).
 
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
 
create_room(RoomName) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    io:format("~p create room ~p~n", [self(),Pid]),
    roomContr:join(RoomName, Pid),
    Pid.
%% @doc  just a test
join(RoomPid, ChatPid) ->
    gen_server:call(RoomPid, {join, ChatPid}).

leave(RoomPid,RoomName,ChatPid) ->
    gen_server:call(RoomPid, {leave, RoomName,ChatPid}).

send(Name, RoomPid, Message) ->
    gen_server:call(RoomPid, {cast, Name, Message}).

broadcast(RoomPid, UserName, Message) ->
    gen_server:call(RoomPid, {broadcast, UserName, Message}).

hello(Pid) ->
    gen_server:cast(Pid, 'HELLO').

% print() ->
%     gen_server:call(RoomPid, )

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
 
init(Args) ->
    io:format("room init~n"),
    {ok, Args}.

handle_call({join, ChatPid}, _From, State) ->
    % io:format("chat ~p join chatroom ~p~n", [ChatPid, self()]),
    {reply, ok, [ChatPid|State]};

handle_call({leave, RoomName,ChatPid}, _From, State) ->
    % io:format("chat ~p leave chatroom ~p~n", [ChatPid, self()]),
    NewState = lists:delete(ChatPid, State),
    case NewState of
        [] ->
            io:format("no one in chatroom"),
            roomContr:delete(RoomName);
        _ ->
            done
    end,
    {reply, ok, NewState};

handle_call({broadcast, UserName, Message}, _From, State) ->
    io:format("broadcast: ~p~n",[State]),
    send_message({broadcast, UserName, Message}, State),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast('HELLO', State) ->
    io:format("Hello World!~n"),
    {noreply, State};

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

send_message({broadcast, UserName, Message}, [Pid | T]) ->
    Pid ! {broadcast, UserName, Message},
    send_message({broadcast, UserName, Message}, T);

send_message({broadcast, UserName, Message}, []) ->
    done.
