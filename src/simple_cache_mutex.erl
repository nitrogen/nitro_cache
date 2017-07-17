%%%-------------------------------------------------------------------
%%% @author Jesse Gumm
%%% @copyright (C) 2017, Jesse Gumm
%%% @doc
%%%
%%% @end
%%% Created : 2017-07-16 18:11:59.402630
%%%-------------------------------------------------------------------
-module(simple_cache_mutex).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


-export([
    lock/2,
    lock/1,
    free/1
]).

-define(SERVER, ?MODULE).

-record(state, {mutexes, waitlists}).
-record(mutex, {key, pid, monitor}).




%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


lock(Key) ->
    lock(Key, 0).

lock(Key, Timeout) ->
    Pid = self(),
    CanBlock = Timeout > 0,
    case gen_server:call(?SERVER, {lock, CanBlock, Key, Pid}) of
        success -> success;
        fail -> fail;
        queued ->
            receive
                lock_received -> success
            after Timeout ->
                fail
            end
    end.

free(Key) ->
    Pid = self(),
    gen_server:call(?SERVER, {free, Key, Pid}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{mutexes=[], waitlists=dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |;
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({lock, CanBlock, Key, Pid}, _From, State=#state{mutexes=Mutexes}) ->
    case lists:keymember(Key, #mutex.key, Mutexes) of
        true ->
            case CanBlock of
                true ->
                    NewState = add_waitlist(Key, Pid, State),
                    {reply, queued, NewState};
                false ->
                    {reply, fail, State}
            end;
        false ->
            {Reply, NewState} = do_lock(Key, Pid, State),
            {reply, Reply, NewState}
    end;
handle_call({free, Key, Pid}, _From, State=#state{}) ->
    {Reply, NewState} = do_free(Key, Pid, State),
    NewState2 = issue_and_notify_waitlist_lock(Key, NewState),
    {reply, Reply, NewState2}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', MonRef, _, _, _}, State=#state{mutexes=Mutexes}) ->
    case lists:keyfind(MonRef, #mutex.monitor, Mutexes) of
        #mutex{key=Key} ->
            NewMutexes = lists:keydelete(MonRef, #mutex.monitor, Mutexes),
            NewState = State#state{mutexes=NewMutexes},
            NewState2 = issue_and_notify_waitlist_lock(Key, NewState),
            {noreply, NewState2};
        false ->
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {   ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_lock(Key, Pid, State = #state{mutexes=Mutexes}) ->
    case issue_mutex(Key, Pid) of
        Mutex=#mutex{} ->
            NewMutexes = [Mutex | Mutexes],
            NewState = State#state{mutexes=NewMutexes},
            {success, NewState};
        none ->
            {fail, State}
    end.

issue_mutex(Key, Pid) ->
    MonRef = erlang:monitor(process, Pid),
    case erlang:is_process_alive(Pid) of
        true ->
            #mutex{
                key=Key,
                pid=Pid,
                monitor=MonRef
            };
        false ->
            erlang:demonitor(MonRef),
            none
    end.

do_free(Key, Pid, State = #state{mutexes=Mutexes}) ->
    case lists:keyfind(Key, #mutex.key, Mutexes) of
        #mutex{pid=Pid, monitor=MonRef} ->
            erlang:demonitor(MonRef),
            NewMutexes = lists:keydelete(Key, #mutex.key, Mutexes),
            NewState = State#state{mutexes=NewMutexes},
            {success, NewState};
        _ ->
            {fail, State}
    end.

add_waitlist(Key, Pid, State) ->
    Waitlists = State#state.waitlists,
    Waitlist = case dict:find(Key, Waitlists) of
        error -> queue:new();
        {ok, W} -> W
    end,
    NewWaitlist = queue:in({Key, Pid}, Waitlist),
    NewWaitlists = dict:store(Key, NewWaitlist, Waitlists),
    State#state{waitlists=NewWaitlists}.

get_queued_lock(Key, State = #state{waitlists=Waitlists}) ->
    case dict:find(Key, Waitlists) of
        error ->
            none;
        {ok, Waitlist} ->
            case queue:out(Waitlist) of
                {{value, {Key, Pid}}, NewWaitlist} -> 
                    NewWaitlists = dict:store(Key, NewWaitlist, Waitlists),
                    NewState = State#state{waitlists=NewWaitlists},
                    {Pid, NewState};
                {empty, _} ->
                    none
            end
    end.
    


issue_and_notify_waitlist_lock(Key, State) ->
    case get_queued_lock(Key, State) of
        none -> State;
        {Pid, DequeuedState} ->
            case do_lock(Key, Pid, DequeuedState) of
                {fail, NewState} ->
                    issue_and_notify_waitlist_lock(Key, NewState);
                {success, NewState} ->
                    Pid ! lock_received,
                    NewState
            end
    end.
