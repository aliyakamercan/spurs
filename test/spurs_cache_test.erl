-module(spurs_cache_test).

-include_lib("eunit/include/eunit.hrl").

cache_test_() ->
    {setup,
     fun() -> spurs_cache:new() end,
     fun(_) -> spurs_cache:destroy() end,
     fun all/1 }.

all(_) ->
    [
        ?_assertEqual({ok, <<"123">>}, spurs_cache:put_new(<<"123">>, dict:new())),
        ?_assertEqual({error, key_already_exists}, spurs_cache:put_new(<<"123">>, dict:new())),
        ?_assertEqual(true, spurs_cache:set(<<"123">>, dict:store(name, spurs,
                                                                  dict:new()))),
        ?_assertEqual(true, spurs_cache:set(<<"123">>, dict:store(title, manager,
                                                                  dict:new()))),
        ?_assertEqual(dict:store(name, spurs,
                                 dict:store(title, manager,
                                           dict:new())), 
                     spurs_cache:get(<<"123">>, 1)),
        ?_assertEqual(expired, spurs_cache:get(<<"123">>, 0)),
        ?_assertEqual(not_found, spurs_cache:get(<<"123">>, 20)),
        ?_assertEqual({ok, <<"123">>}, spurs_cache:put_new(<<"123">>, dict:new())),
        ?_assertEqual(dict:new(), spurs_cache:get(<<"123">>, 20)),
        ?_assertEqual(true, spurs_cache:delete(<<"123">>)),
        ?_assertError({badmatch, []}, spurs_cache:set(<<"123">>, [{a,b}]))
    ].
