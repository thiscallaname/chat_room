-module(user_table).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
 
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
 
-export([start/0, sign/1, lookup/1]).
 
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
 
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
%% @doc  just a test
sign(UserName) ->
    gen_server:call(?SERVER, {sign, UserName}).

lookup(UserId) ->
    gen_server:call(?SERVER, {lookup, UserId}).

change_name(UserId, UserName) ->
    gen_server:call(?SERVER, {change_name, UserName, UserId}).

delete(UserId) ->
    gen_server:call(?SERVER, {delete, UserId}).

close() ->
    gen_server:call(?SERVER, {close}).
 
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
 
init(Args) ->
    {ok, User_table} = dets:open_file("userTable", []),
    {ok, User_table}.

handle_call({sign, UserName}, _From, State) ->
    UserId = index(1, State),
    dets:insert(State, {UserId, UserName}),
    {reply, {ok, UserId}, State};

handle_call({lookup, UserId}, _From, State) ->
    [{UserId, UserName}] = dets:lookup(State, UserId),
    {reply, {ok, UserName}, State};

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

index(X, L) ->
    case dets:member(L, X) of
        true ->
            % io:format("~w~n", X);
            index(X+1, L);
        false ->
            X
    end.