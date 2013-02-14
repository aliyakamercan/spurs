-module(toppage_handler2).

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
    %% set_p persists changes immidiately so no need to save the state upon 
    %% terminate
    State2 = spurs:set_p(State, last_visit, Now),
    {ok, Req1} = cowboy_req:reply(200,
                                  [{<<"content-type">>, <<"text/html">>}],
                                  Body, Req),
    {ok, Req1, State2}.

terminate(_Reason, _Req, _State) ->
    ok.
