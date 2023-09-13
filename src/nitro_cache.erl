%%% @doc Main module for nitro_cache.
%%%
%%% Copyright 2013 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(nitro_cache).
-author('marcelog@gmail.com').
-include("nitro_cache.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ETS_TID, "nitro_cache_").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([init/1]).
-export([cache_exists/1]).
-export([get/4]).
-export([get_only/2]).
-export([set/4]).
-export([flush/1, flush/2, flush/3]).

-export([benchmark/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initializes a cache.
-spec init(atom()) -> ok.
init(CacheName) ->
  case gen_server:call(nitro_cache_expirer, {new, CacheName}) of
    ok -> ok;
    {error, Error} ->
      error_logger:error_msg("Failed to initialize a new cache.~nName: ~p.~nError Message: ~p",[CacheName, Error]),
      throw({failed_to_init_cache, CacheName})
  end.

-spec cache_exists(atom()) -> boolean().
cache_exists(CacheName) ->
	RealName = ?NAME(CacheName),
	lists:member(RealName, ets:all()).

%% @doc Deletes the keys that match the given ets:matchspec() from the cache.
-spec flush(atom(), term()) -> true.
flush(CacheName, Key) ->
  RealName = ?NAME(CacheName),
  ets:delete(RealName, Key).

%% @doc Deletes all keys in the given cache.
-spec flush(atom()) -> true.
flush(CacheName) ->
  RealName = ?NAME(CacheName),
  true = ets:delete_all_objects(RealName).

%% @doc Deletes a key from cache only if it still has the expected Expiry
%% value.  This is used by the expirer when a key is due to expire.  This
%% avoids inadvertently deleting a value that has since been updated (it
%% would thus have a different Expiry).
-spec flush(atom(), term(), pos_integer()) -> true.
flush(CacheName, Key, Expiry) ->
  RealName = ?NAME(CacheName),
  try ets:match_delete(RealName, {Key, '_', Expiry})
  catch error:badarg ->
    error_logger:warning_msg("Trying to Expire ~p with Expiry ~p from ~p failed. Maybe the ETS table was deleted.",[Key, Expiry, RealName]),
    true
  end.

%% @dor Tries to lookup Key in the cache, and execute the given FunResult
%% on a miss.
-spec get(atom(), infinity|integer(), term(), function()) -> term().
get(CacheName, LifeTime, Key, FunResult) ->
    get(CacheName, LifeTime, Key, FunResult, 0).

get(CacheName, LifeTime, Key, FunResult, 10) ->
    throw({nitro_cache_failed, CacheName, LifeTime, Key, FunResult});

get(CacheName, LifeTime, Key, FunResult, _TimesChecked) when LifeTime =< 0 ->
  case get_only(CacheName, Key) of
    undefined -> FunResult();
    V -> V
  end;
get(CacheName, LifeTime, Key, FunResult, TimesChecked) ->
  RealName = ?NAME(CacheName),
  try ets:lookup(RealName, Key) of
    [] ->
      MutexName = {CacheName, Key},
      case mutagen:lock(MutexName) of
          success ->
              V = FunResult(),
              set(CacheName, LifeTime, Key, V),
              mutagen:free(MutexName),
              V;
          fail ->
              %% Since the mutex is not available, let's wait until it frees up, then try again.
              case mutagen:wait(MutexName, 10000) of
                free ->
                    ok;
                not_free ->
                    logger:warning("Nitro Cache: Mutex \"~p\" timed out after 10 seconds. Something might be wrong. Trying again.", [MutexName])
              end,
              get(CacheName, LifeTime, Key, FunResult, TimesChecked+1)
       end;
    [{Key, R, _Expiry}] -> R % Found, return the value.
  catch
    error:badarg ->
      case cache_exists(CacheName) of
        true ->
            %% It's possible the cache was initialized between first doing the lookup and here. So let's just try again
            get(CacheName, LifeTime, Key, FunResult, TimesChecked+1);
        false ->
          init(CacheName),
          get(CacheName, LifeTime, Key, FunResult)
      end
  end.

%% @doc Tries to lookup Key in the cache, and execute the given FunResult
%% on a miss.
-spec get_only(atom(), term()) -> undefined|term().
get_only(CacheName, Key) ->
  RealName = ?NAME(CacheName),
  try ets:lookup(RealName, Key) of
    [] -> undefined;
    [{Key, R, _Expiry}] -> R % Found, return the value.
  catch
    error:badarg -> undefined
  end.

%% @doc Sets Key in the cache to the given Value
-spec set(atom(), infinity|pos_integer(), term(), term()) -> ok.
set(CacheName, LifeTime, Key, Value) ->
  RealName = ?NAME(CacheName),
  %% Store the expiry time on the entry itself so that the expirer won't
  %% accidentally expire the entry early if it gets updated between now
  %% and when the expiration is first scheduled to occur.
  %io:format("Settings ~p => ~p~n",[Key, Value]),
  case LifeTime of
    infinity ->
      ets:insert(RealName, {Key, Value, infinity});
    _ ->
      Expiry = now_millis() + LifeTime,
      ets:insert(RealName, {Key, Value, Expiry}),
      erlang:send_after(
        LifeTime, nitro_cache_expirer, {expire, CacheName, Key, Expiry}
       )
  end,
  ok.

now_millis() ->
    {Mega, Seconds, Micro} = os:timestamp(),
    (Mega * 1000000 + Seconds) * 1000 + trunc(Micro/1000).


benchmark(NumKeys) ->
    GenFun = fun() -> timer:sleep(1000), ok end,
    io:format("Starting Nitro Cache ~n"),
    application:ensure_all_started(nitro_cache),
    io:format("Generating ~p Keys~n", [NumKeys]),
    Keys = lists:seq(1, NumKeys),
    MaxRequestTimes = 1000,
    RequestTimes = lists:seq(1, MaxRequestTimes),
    io:format("Generating ~p values with a 1000ms delay. Then immediately requesting each 1000 times in parallel~n", [NumKeys]),
    ListOfGetTimes = pmap(fun(K) ->
        erlang:spawn(fun() ->
            nitro_cache:get(benchmark, 60000, K, GenFun)
        end),
        WrongFun = fun() -> K end,
        {_GetTime, _Returns} = timer:tc(fun() ->
            pmap(fun(_) ->
                nitro_cache:get(benchmark, 0, K, WrongFun) %% WrongFun should never be run unless it takes too long to process.
            end, RequestTimes)
        end)
    end, Keys),
    GetTimes = [GT || {GT, _} <- ListOfGetTimes],
    Returns = lists:flatten([R || {_, R} <- ListOfGetTimes]),
    NotOK = [X || X <- Returns, X =/= ok],
    case NotOK of
        [] ->
            io:format("There were ~p items that failed to return the expected value 'ok'~n",[length(NotOK)]);
        _ ->
            io:format("All items returned the expected value~n")
    end,
    AvgWorkTime = lists:sum(GetTimes) / (length(GetTimes) * MaxRequestTimes),
    
    io:format("Total Work Time was ~p microseconds per request", [AvgWorkTime]).


%Joe Armstrong's pmap implementation
%pmap is a parallel map
pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) ->
        spawn(fun() -> do_f(S, F, I) end)
    end, L),
    gather(Pids).

gather([H|T]) ->
     receive
         {H, Ret} -> [Ret|gather(T)]
     end;
 gather([]) ->
     [].

 do_f(Parent, F, I) ->
     Parent ! {self(), (catch F(I))}.

