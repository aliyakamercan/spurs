-module(spurs_ex_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/1", toppage_handler1, []},
			{"/2", toppage_handler2, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
        ,{middlewares, [cowboy_router, spurs_middleware, cowboy_handler]}
	]),
    spurs_ex_sup:start_link().

stop(_State) ->
    ok.
