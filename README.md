Spurs
=====

A session manager for cowboy. 

Usage
-----

Add it to your middleware chain before handler middleware and it will append all session details to handler_opts. From there you can get or set session values.

Example
------

####Adding to middleware chain:

```erlang
    
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
        ,{middlewares, [cowboy_router, spurs_middleware, cowboy_handler]}
	]),

```

####Handler:

```erlang

-module(toppage_handler1).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Opts) ->
    %% pass the opts as state
    {ok, Req, Opts}.

handle(Req, State) ->
    Now = now(),
    %% get the value from session
    Body = case spurs:get(State, last_visit) of
        undefined ->
            <<"Hey! It is your first visit. Try refreshing in a few seconds.">>;
        TS ->
            Diff = timer:now_diff(Now, TS) / 1000000,
            [<<"Nice to see you again, your last visit was ">>, 
             list_to_binary(integer_to_list(trunc(Diff))), 
             <<" seconds ago.">>]
    end,
    %% set a new value (this is not yet persisted)
    State2 = spurs:set(State, last_visit, Now),
    {ok, Req1} = cowboy_req:reply(200,
                                  [{<<"content-type">>, <<"text/html">>}],
                                  Body, Req),
    %% don't forget to return the new state
    {ok, Req1, State2}.

terminate(_Reason, _Req, State) ->
    %% save your changes
    spurs:save(State),
    ok.
    
```

Options
--------
You can set the following options:

```erlang

[
    {spurs, [
        {expiry, 3600 * 24 * 3} % session expiration time in seconds - defaults to 3 days
        ,{cookie_opt, []} % cookie options - defaults to [{http_only, true}, {path, <<"/">>}]
        ,{backend, spurs_cache} % backend - defaults to spurs_cache --see todo
    ]}
].

```

TODO
----

1) Add mnesia and gproc backend and test in a distributed environment. 

2) Performance tests & improvement

