-module(spurs_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = set_defaults(), 
    {ok, Storage} = application:get_env(spurs, backend),
    Storage:new(), 
    spurs_sup:start_link().

stop(_State) ->
    ok.


%internal 

set_defaults() ->
    case application:get_env(backend) of
        undefined -> application:set_env(spurs, backend, spurs_cache);
        {ok, _} -> ok
    end,
    case application:get_env(expiry) of
        undefined -> application:set_env(spurs, expiry, 3600 * 24 * 3);
        {ok, _} -> ok
    end,
    case application:get_env(cookie_opts) of
        undefined -> application:set_env(spurs, cookie_opts, [{http_only, true},
                                                              {path, <<"/">>}]);
        {ok, _} -> ok
    end,
    ok.
