-module(spurs_session_test).

-include_lib("eunit/include/eunit.hrl").

session_test_() ->
    {setup,
     fun() -> spurs:start() end,
     fun(_) -> application:stop(spurs) end,
     fun(_) ->
                [get_delete(),
                 save_get(),
                 expiry()]
        end  }.

get_delete() ->
    Id = spurs_session:create(dict:new()),
    [
        ?_assertEqual(36, size(Id)),
        ?_assert(is_binary(Id)),
        ?_assertEqual(dict:new(), spurs_session:get(Id)),
        ?_assertEqual(not_found, spurs_session:get(<<"random_id">>)),
        ?_assertEqual(true, spurs_session:delete(Id)),
        ?_assertEqual(not_found, spurs_session:get(Id))
    ].

save_get() ->
    Id = spurs_session:create(dict:new()),
    Dict1 = dict:store(key1, value1, dict:new()),
    Dict2 = dict:store(key2, value2, Dict1),
    [
        ?_assertEqual(true, spurs_session:save(Id, Dict1)),
        ?_assertEqual(dict:to_list(Dict1), dict:to_list(spurs_session:get(Id))),
        ?_assertEqual(true, spurs_session:save(Id, Dict2)),
        ?_assertEqual(dict:to_list(Dict2), dict:to_list(spurs_session:get(Id)))
    ].

expiry() ->
    application:set_env(spurs, expiry, 1),
    Id = spurs_session:create(dict:new()),
    [
        ?_assertEqual(dict:new(), spurs_session:get(Id)),
        ?_assertEqual(ok, timer:sleep(1000)),
        ?_assertEqual(expired, spurs_session:get(Id)),
        ?_assertEqual(not_found, spurs_session:get(Id))
    ].
