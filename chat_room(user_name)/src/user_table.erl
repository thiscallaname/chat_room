-module(user_table).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
 
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
 
-export([start_link/0,add/1,list_all_user/0,change_name/2,delete/1,lookup/1]).
 
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
 
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
%% @doc 
 
add(Name) ->
    % io:format("add~n"),
	gen_server:call(?MODULE, {add,Name}).

lookup(UserId) ->
    gen_server:call(?MODULE, {lookup, UserId}).


change_name(UserId, Name) ->
	gen_server:call(?MODULE, {change, UserId, Name}).

list_all_user() ->
	gen_server:call(?MODULE, {listAll}).

delete(UserId) ->
    gen_server:call(?MODULE, {delete, UserId}).

close() ->
    gen_server:call(?MODULE, {close}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
 
init([]) ->
	{ok,User_table} = dets:open_file("userTable", []),
    % io:format("~w~n", [?MODULE]),
    {ok, User_table}.

handle_call({add, Name}, _From, State) ->
	[Index] = index(1,State),
    io:format("~p~n", [Index]),
	dets:insert(State, {Index, Name}),
    % {Pid, Tag} = _From,
    % Pid ! {index, Index},
    {reply, {index, Index}, State};

handle_call({listAll}, _From, State) ->
	dets:traverse(State,fun(X) -> io:format("~p~n", [X]), continue end),
    {reply, ok, State};

handle_call({delete, UserId}, _From, State) ->
    dets:delete(State, UserId),
    {reply, ok, State};

handle_call({lookup, UserId}, _From, State) ->
    case dets:lookup(State, UserId) of
        {error, Reason} ->
            io:format("~p: error", [?MODULE]),
            {reply, {error}, State};
        [{ID, Name}] ->
            {reply, {ok,Name}, State};
        R ->
            io:format("~p: empty", [?MODULE]),
            {reply, {empty}, State}

    end;

handle_call({close}, _From, State) ->
    dets:close(State),
    {reply, ok, State};

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


index(X, L) ->
    case dets:member(L, X) of
        true ->
            % io:format("~w~n", X);
            index(X+1, L);
        false ->
            [X]
    end.