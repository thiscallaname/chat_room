-module(m).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
-export([start/0, join/2, leave/1, cast/1, send_message/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port}).
-define(SERVER, ?MODULE).

start() ->
    % io:format("m: start~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
  
join(Name, Pid) ->
    % io:format("m: join~w~n", [Pid]),
    gen_server:call(?MODULE, {join,Name, Pid}).

leave(Pid) ->
    gen_server:call(?MODULE, {leave, Pid}).

cast([Name,Message]) ->
    gen_server:cast(?MODULE, {cast,Name,Message}).%,
    % io:format("m:cast ~n").


% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) ->
    % io:format("~p~p~n", [?LINE, self()]),
    {ok, []}.

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
% handle_call(Request, From, State) ->
%     Reply = ok,
%     {reply, Reply, State}.
handle_call({recv, Message}, From, State) ->
    % io:format("test~n"),
    Reply = ok,
    {reply, Reply, State};

handle_call({join,Name, Pid}, From, State) ->
    io:format("~p join chat room: ~n", [Name]),
    Reply = ok,
    {reply, Reply, [Pid | State]};    
    % server:start(Listen);

handle_call({leave, Pid}, From, State) ->
    io:format("m leave: ~w~n", [Pid]),
    Reply = ok,
    NewState = lists:delete(Pid, State),
    {reply, Reply, NewState};

handle_call(Info, From, State) ->
    % io:format("test~n"),
    Reply = ok,
    {reply, Reply, State}. 
% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast({cast,Name,Message}, State) ->
    m:send_message({cast,Name,Message}, State),
    % io:format("cast:~p~p~p~n", [Name,Message,State]),
    {noreply, State};

handle_cast(Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info({a, Message}, State) ->
    % io:format("handle_info ~p~n",Message),
    {noreply, State};

handle_info(Info, State) ->
    % io:format("Info~n"),
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


send_message(Message, [Pid | T]) ->
    Pid ! Message,
    % io:format("~p~p~n", [Message, Pid]).
    send_message(Message, T);

send_message(Message, []) ->
    % io:format("~p~n", [Message]),
    done.