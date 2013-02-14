-module(toppage_handler1).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Opts) ->
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
