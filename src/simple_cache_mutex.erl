%%%-------------------------------------------------------------------
%%% @author Jesse Gumm
%%% @copyright (C) 2017, Jesse Gumm
%%% @doc This is a mutex server for simple_cache. There are no pre-set mutexes. Simply requesting a mutex with lock/1 or lock/2 will either issue a mutex or return "fail" if the mutex is not free, or if you set a timeout, it will wait until it frees.
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
    free/1,
    wait/1,
    wait/2
]).

-define(SERVER, ?MODULE).

-record(state, {mutexes, waitlists, notifylists}).
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
        {queued, Ref} ->
            receive
                {lock_received, Ref} -> success
            after Timeout ->
                fail
            end
    end.

free(Key) ->
    Pid = self(),
    gen_server:call(?SERVER, {free, Key, Pid}).

wait(Key) ->
    wait(Key, infinity).

wait(Key, Timeout) ->
    Pid = self(),
    case  gen_server:call(?SERVER, {wait, Key, Pid}) of
        {waiting, Ref} ->
            receive
                {free, Ref} -> free
            after
                Timeout -> not_free
            end;
        free ->
            free
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{mutexes=[], waitlists=dict:new(), notifylists=dict:new()}}.

handle_call({lock, CanBlock, Key, Pid}, _From, State) ->
    case is_locked(Key, State) of
        true ->
            case CanBlock of
                true ->
                    {Ref, NewState} = add_waitlist(Key, Pid, State),
                    {reply, {queued, Ref}, NewState};
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
    NewState3 = send_notify_lists(Key, NewState2),
    {reply, Reply, NewState3};

handle_call({wait, Key, Pid}, _From, State=#state{}) ->
    case is_locked(Key, State) of
        true ->
            {Ref, NewState} = add_notify(Key, Pid, State),
            {reply, {waiting, Ref}, NewState};
        false ->
            {reply, free, State}
    end.

is_locked(Key, #state{mutexes=Mutexes}) ->
    lists:keymember(Key, #mutex.key, Mutexes).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, _, _, _}, State=#state{mutexes=Mutexes}) ->
    case lists:keyfind(MonRef, #mutex.monitor, Mutexes) of
        #mutex{key=Key} ->
            NewMutexes = lists:keydelete(MonRef, #mutex.monitor, Mutexes),
            NewState = State#state{mutexes=NewMutexes},
            NewState2 = issue_and_notify_waitlist_lock(Key, NewState),
            NewState3 = send_notify_lists(Key, NewState2),
            {noreply, NewState3};
        false ->
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
        ok.

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
    Ref = erlang:make_ref(),
    NewWaitlist = queue:in({Key, Pid, Ref}, Waitlist),
    NewWaitlists = dict:store(Key, NewWaitlist, Waitlists),
    {Ref, State#state{waitlists=NewWaitlists}}.


add_notify(Key, Pid, State) ->
    Notifylists = State#state.notifylists,
    Notifylist = case dict:find(Key, Notifylists) of
        error -> [];
        {ok, N} -> N
    end,
    Ref = make_ref(),
    NewNotifylist = [{Pid, Ref} | Notifylist],
    NewNotifylists = dict:store(Key, NewNotifylist, Notifylists),
    {Ref, State#state{notifylists=NewNotifylists}}.

get_queued_lock(Key, State = #state{waitlists=Waitlists}) ->
    case dict:find(Key, Waitlists) of
        error ->
            none;
        {ok, Waitlist} ->
            case queue:out(Waitlist) of
                {{value, {Key, Pid, Ref}}, NewWaitlist} -> 
                    NewWaitlists = dict:store(Key, NewWaitlist, Waitlists),
                    NewState = State#state{waitlists=NewWaitlists},
                    {Pid, Ref, NewState};
                {empty, _} ->
                    none
            end
    end.
    
issue_and_notify_waitlist_lock(Key, State) ->
    case get_queued_lock(Key, State) of
        none -> State;
        {Pid, Ref, DequeuedState} ->
            case do_lock(Key, Pid, DequeuedState) of
                {fail, NewState} ->
                    issue_and_notify_waitlist_lock(Key, NewState);
                {success, NewState} ->
                    Pid ! {lock_received, Ref},
                    NewState
            end
    end.

send_notify_lists(Key, State = #state{notifylists=Notifylists}) ->
    case dict:find(Key, Notifylists) of
        error ->
            State;
        {ok, Notifylist} ->
            lists:foreach(fun({Pid, Ref}) ->
                Pid ! {free, Ref}
            end, Notifylist),
            NewNotifylists = dict:erase(Key, Notifylists),
            State#state{notifylists=NewNotifylists}
    end.
