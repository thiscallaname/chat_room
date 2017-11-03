-module(m).

-behaviour(gen_server).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------

% --------------------------------------------------------------------
% External exports
-export([start/0, join/1, leave/1, cast/1, send_message/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port}).
-define(SERVER, ?MODULE).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

join(Pid) ->
    gen_server:call(?MODULE, {join, Pid}).

leave(Pid) ->
    gen_server:call(?MODULE, {leave, Pid}).

cast([Name,Message]) ->
    gen_server:cast(?MODULE, {cast,Name,Message}).%,

send_message(Message, [Pid | T]) ->
    Pid ! Message,
    send_message(Message, T);

send_message(Message, []) ->
    io:format("~p~n", [Message]),
    done.
% --------------------------------------------------------------------

init([]) ->
    {ok, []}.


handle_call({recv, Message}, From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call({join,Pid}, From, State) ->
    Reply = ok,
    {reply, Reply, [Pid | State]};
    % server:start(Listen);

handle_call({leave, Pid}, From, State) ->
    Reply = ok,
    _State = lists:delete(Pid, State),
    {reply, Reply, _State};

handle_call(Info, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({cast,Name,Message}, State) ->
    m:send_message({cast,Name,Message}, State),
    {noreply, State};

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info({a, Message}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
