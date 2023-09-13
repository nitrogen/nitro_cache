-module(nitro_cache_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(mutagen),
    nitro_cache_sup:start_link().

stop(_State) ->
    ok.
