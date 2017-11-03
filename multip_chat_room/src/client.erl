-module(client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
 
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
 
-export([login/1, sign/1, send/2,list/0, create_room/1, join/1, leave/1]).
 
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
 

 
%% @doc  just a test
% join() ->
%     gen_server:call(?MODULE, )
login(UserID) ->
    start(),
    gen_server:call(?MODULE, {login, UserID}).

sign(UserName) ->
    start(),    
    gen_server:call(?MODULE, {sign, UserName}).

send(Message, RoomName) ->
    gen_server:call(?MODULE, {send, Message, RoomName}).

create_room(RoomName) ->
    gen_server:call(?MODULE, {create, RoomName}),
    join(RoomName).

join(RoomName) ->
    gen_server:call(?MODULE, {join, RoomName}).

leave(RoomName) ->
    gen_server:call(?MODULE, {leave, RoomName}).

list() -> 
    gen_server:call(?MODULE, {list}).


 
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
 
init(Args) ->
    {ok, Socket} = gen_tcp:connect("localhost", 6789, [binary, {packet, 4}]),   
    {ok, Socket}.

handle_call({login, UserID}, _From, State) ->
    gen_tcp:send(State, term_to_binary({login,UserID})),
    {reply, ok, State};

handle_call({sign, UserName}, _From, State) ->
    gen_tcp:send(State, term_to_binary({sign,UserName})),
    {reply, ok, State};

handle_call({send,Message, RoomName}, _From, State) ->
    gen_tcp:send(State, term_to_binary({send,Message, RoomName})),
    {reply, ok, State};

handle_call({list}, _From, State) ->
    gen_tcp:send(State, term_to_binary(list)),
    {reply, ok, State};

handle_call({create, RoomName}, _From, State) ->
    gen_tcp:send(State, term_to_binary({create, RoomName})),
    {reply, ok, State};

handle_call({join, RoomName}, _From, State) ->
    gen_tcp:send(State, term_to_binary({join, RoomName})),
    {reply, ok, State};

handle_call({leave, RoomName}, _From, State) ->
    gen_tcp:send(State, term_to_binary({leave, RoomName})),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 

handle_info({tcp, Socket, Bin}, State) ->
    case binary_to_term(Bin) of
        {all, All} ->
            listAll(All);
        {broadcast, Name, Message} ->
            io:format("~p: ~p~n", [Name, Message]);
        {id, UserID} ->
            io:format("sign up success, your id is ~p~n", [UserID]);
        {login, UserName} ->
            io:format("login in success, nickname is ~p~n", [UserName]);
        _ ->
            io:format("client recv~n")
    end,
    {noreply, State};

handle_info(_Info, State) ->
    io:format("client info ~p~n", [_Info]),
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
listAll([{Name, Pid}|T]) -> 
    io:format("~p ~n", [Name]),
    listAll(T);

listAll([]) -> ok.

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).