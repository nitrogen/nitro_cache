# NitroCache

NitroCache is a fork of [simple_cache](https://github.com/marcelog/simple_cache)
modified specifically for the needs of integration with the [Nitrogen Web
Framework](http://nitrogenproject.com).

The updated code now has a separate process to handle the expirations, which is
a regular `gen_server`. To start it, just do:

```erlang
nitro_cache_expirer:start_link().
```

## Create a cache

```erlang
nitro_cache:init(my_cache_name).
```

## Getting a key

The following call will lookup **my_key** in the cache named **my_cache_name**, and on
a MISS will call the given **fun**, caching its result for **3600000** milliseconds.

```erlang
nitro_cache:get(my_cache_name, 3600000, my_key, fun() ->
	io:format("This fun will be called on a cache miss~n"),
	timer:sleep(5000)
	this_value_will_be_cached
end).
```

## Flushing the cache

```erlang
    nitro_cache:flush(my_cache_name).
```

## Flushing a key

```erlang
    nitro_cache:flush(my_cache_name, my_key).
```

## Changes:

### Version 0.5.0

+ The mutex server has been pulled out of NitroCache and renamed to
  [Mutagen](https://github.com/nitrogen/mutagen).

### Version 0.4.1

+ Fixed a bug where the mutex wouldn't free if the provided function crashed.

### Version 0.4.0

+ Renamed to NitroCache to avoid a name clash (simple_cache is already taken
  on hex.pm)
+ Requesting a key from a bucket if it has not yet been initialized will
  automatically instantiate that bucket.
+ NitroCache introduced a mutex system to prevent recalculating the same
  slow-calculating key when multiple requests come in at same time.  When the
  key's value is calculated, it will return the result to all the waiting
  requests.
+ Added a `cache_exists/1` function.
+ Added a `set/4` function.
+ Converted to rebar3

### Earlier Version Modifications from the [original](https://github.com/marcelog/simple_cache)

* Requesting a key from a bucket if it has not yet been initialized will
  automatically instantiate that bucket.
* Added a `cache_exists/1` function.
* Added a `set/4' function.
