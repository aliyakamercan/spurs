-module(spurs_ex).

-export([start/0]).

start() ->
    start_app(spurs_ex).

start_app(App) ->
    case application:start(App) of
        ok -> ok;
        {error,{already_started,App}} -> ok;
        {error,{not_started,Dep}} ->
            start_app(Dep),
            start_app(App)
    end.
