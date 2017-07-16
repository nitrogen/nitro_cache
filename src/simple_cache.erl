%%% @doc Main module for simple_cache.
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
-module(simple_cache).
-author('marcelog@gmail.com').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ETS_TID, atom_to_list(?MODULE)).
-define(NAME(N), list_to_atom(?ETS_TID ++ "_" ++ atom_to_list(N))).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initializes a cache.
-spec init(atom()) -> ok.
init(CacheName) ->
  RealName = ?NAME(CacheName),
  Config = [
    named_table,
    {read_concurrency, true},
    public,
    {write_concurrency, true}
  ],
  try ets:new(RealName, Config) of
    RealName -> 
      %% A little nasty. Make it start directly on the expirer
      ets:give_away(RealName, erlang:whereis(simple_cache_expirer), undefined),
      ok
  catch error:badarg ->
    %% Table might have been created already. Let's check if it exists.
    case cache_exists(CacheName) of
      true -> ok;
      false ->
        error_logger:error_msg("Failed to initialize a new Cache called ~p. Attempted to create it, received an error, checked to see if it was already created, and it wasn't already created. So something is awry.",[CacheName]),
        throw({failed_to_init_cache, CacheName})
    end
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
-spec get(atom(), infinity|pos_integer(), term(), function()) -> term().
get(CacheName, LifeTime, Key, FunResult) ->
    get(CacheName, LifeTime, Key, FunResult, 0).

get(CacheName, LifeTime, Key, FunResult, 50) ->
    throw({simple_cache_failed, CacheName, LifeTime, Key, FunResult});

get(CacheName, LifeTime, Key, FunResult, TimesChecked) ->
  RealName = ?NAME(CacheName),
  try ets:lookup(RealName, Key) of
    [] ->
      % Not found, create it.
      SettingKey = {setting_cache_key, Key},
      case ets:lookup(RealName, SettingKey) of
         [] -> 
             ets:insert(RealName, {SettingKey, true}),
             try
                V = FunResult(),
                set(CacheName, LifeTime, Key, V),
                ets:delete(RealName, SettingKey),
                V
             catch
                Error:Type ->
                    ets:delete(RealName, SettingKey),
                    exit({Error, Type, erlang:get_stacktrace()})
             end;
         [{SettingKey, true}] ->
             %io:format("~p being processed. Sleeping~n", [SettingKey]),
             timer:sleep(100),
             get(CacheName, LifeTime, Key, FunResult, TimesChecked+1)
       end;
    [{Key, R, _Expiry}] -> R % Found, return the value.
  catch
    error:badarg ->
      case cache_exists(CacheName) of
        true ->
            %% It's possible the cache was initialized between first doing the lookup and here. So let's just try again
            get(CacheName, LifeTime, Key, FunResult, TimesChecked-1);
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
        LifeTime, simple_cache_expirer, {expire, CacheName, Key, Expiry}
       )
  end,
  ok.

now_millis() ->
    {Mega, Seconds, Micro} = os:timestamp(),
    (Mega * 1000000 + Seconds) * 1000 + trunc(Micro/1000).
