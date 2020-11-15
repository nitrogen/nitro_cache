-module(nitro_cache_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
	all/0,
	groups/0,
	init_per_group/2,
	end_per_group/2
]).

-export([
    infinity_fast/1,
    infinity_slow/1,
    timed_fast/1,
    timed_slow/1,
    lots_of_keys/1
]).

all() ->
    [
        {group,'1000'},
        {group,'100000'}
    ].

-define(BUCKET, bucket).

test_to_num(Atom) -> list_to_integer(atom_to_list(Atom)).

config_to_num(Config) ->
    proplists:get_value(num, Config).

groups() ->
	[
        {'1000', 
		    [shuffle, sequence, parallel, {repeat, 10}],
		    [infinity_fast, infinity_slow, timed_fast, timed_slow, lots_of_keys]
	    },
        {'100000', 
		    [shuffle, sequence, parallel],
		    [infinity_fast, infinity_slow, timed_fast, timed_slow, lots_of_keys]
	    }
    ].

init_per_group(Test, Config) ->
    Num = test_to_num(Test),
	application:start(nitro_cache),
	[{num, Num} | Config].

end_per_group(_Test, Config) ->
	application:stop(nitro_cache),
	Config.

running_notice() ->
    io:format("*********** Running Function to Cache ***************~n").

infinity_fast(Config) ->
    Key = ?FUNCTION_NAME,
    Fun = fun() ->
        ok = nitro_cache:get(?BUCKET, infinity, Key, fun() -> running_notice(), ok end)
    end,
    run_times(Fun, config_to_num(Config), 1).


infinity_slow(Config) ->
    Key = ?FUNCTION_NAME,
    Fun = fun() ->
        ok = nitro_cache:get(?BUCKET, infinity, Key, fun() -> running_notice(), timer:sleep(1000), ok end)
    end,
    ok = run_times(Fun, config_to_num(Config), 1).

timed_slow(Config) ->
    Key = ?FUNCTION_NAME,
    Fun = fun() ->
        ok = nitro_cache:get(?BUCKET, 10000, Key, fun() -> running_notice(), timer:sleep(1000), ok end)
    end,
    ok = run_times(Fun, config_to_num(Config), 1).

timed_fast(Config) ->
    Key = ?FUNCTION_NAME,
    Fun = fun() ->
        ok = nitro_cache:get(?BUCKET, 1000, Key, fun() -> running_notice(), ok end)
    end,
    ok = run_times(Fun, config_to_num(Config), 1).

lots_of_keys(Config) ->
    Fun = fun() ->
        Key = rand:uniform(10000),
        Key = nitro_cache:get(?BUCKET, infinity, Key, fun() -> running_notice(), timer:sleep(1000), Key end)
    end,
    ok = run_times(Fun, config_to_num(Config), 1).

run_times(Fun, Times, DelayPerRequest) ->
    times_worker(Fun, Times, DelayPerRequest),
    ok = harvester(Times).

times_worker(_Fun, 0, _Delay) ->
    ok;
times_worker(Fun, Times, DelayPerRequest) ->
    Me = self(),
    case Times rem 10000 of
        0 ->
            error_logger:info_msg("Remaining ~p",[Times]);
        _ ->
            ok
    end,
    erlang:spawn(fun() ->
        try timer:tc(Fun) of
            {Time, Result} ->
                Me ! {result, {Times, Time, Result}}
        catch
            E:T:S ->
                Me ! {error, {Times, {E,T,S}}}
        end
    end),
    timer:sleep(DelayPerRequest),
    times_worker(Fun, Times-1, DelayPerRequest).

harvester(0) ->
    ok;
harvester(Times) ->
    receive
        {result, {Num, Time, Result}} ->
            io:format("Result Number ~p Received: ~p. Time: ~p~n", [Num, Result, Time]),
            harvester(Times-1);
        {error, {Num, Error}} ->
            io:format("ERROR RESULT RECEIVED: ~p~n",[Num]),
            erlang:exit(Error)
    after 20000 ->
        {timeout, Times}
    end.
