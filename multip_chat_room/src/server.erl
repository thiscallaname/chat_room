-module(server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
 
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
 
-export([start/1]).
 
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(server_message,{socket, userid, username}). 
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
 
start(Socket) ->
    io:format("server start~n"),
    {ok, Pid} = gen_server:start_link(?MODULE, [Socket], []),
    {ok, Pid}.


%% @doc  just a test
% join() ->
%     gen_server:call(?MODULE, )

send(Message) ->
    gen_server:call(?MODULE, {send, Message}).

 
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
 
init([Socket]) ->
    io:format("init~n"),
    NewServerMessage = #server_message{socket = Socket},
    {ok, NewServerMessage}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Bin}, State) ->
    io:format("server :~p~n",[binary_to_term(Bin)]),
    Port = State#server_message.socket,
    case binary_to_term(Bin) of 
        list ->
            {ok, All} = roomContr:list(),
            io:format("server: ~p ,~p~n", [State, All]),
            gen_tcp:send(Port, term_to_binary({all,All}));
        {sign, UserName} ->
            {ok, UserId} = user_table:sign(UserName),
            gen_tcp:send(Port, term_to_binary({id, UserId}));
        % {login, UserId} ->
        %     {ok, UserName} = user_table:lookup(UserId),
        %     gen_tcp:send(Port, term_to_binary({login, UserName}));
        {create, RoomName} ->
            room:create_room(RoomName);
        {send, Message, RoomName} ->
            RoomPid = roomContr:find(RoomName),
            UName = State#server_message.username,
            io:format("pid : ~w~n",[RoomPid]),
            room:broadcast(RoomPid, UName,Message);
        {join, RoomName} ->
            RoomPid = roomContr:find(RoomName),
            room:join(RoomPid, self());
        {leave, RoomName} ->
            RoomPid = roomContr:find(RoomName),
            room:leave(RoomPid,RoomName,self());
        _ ->
            io:format("not match in server ~p~n", [?LINE])
    end,

    NewServerMessage = 
        case binary_to_term(Bin) of 
            {login, UId} ->
                {ok, Name} = user_table:lookup(UId),
                gen_tcp:send(Port, term_to_binary({login, Name})),
                State#server_message{userid = UId, username = Name};
            _ ->
                State
        end,
    {noreply, NewServerMessage};    

handle_info({broadcast, Name, Message}, State) ->
    Socket = State#server_message.socket,
    gen_tcp:send(Socket, term_to_binary({broadcast, Name, Message})),
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