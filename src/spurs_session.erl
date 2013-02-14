-module(spurs_session).

-export([create/1,
         delete/1,
         save/2,
         get/1
         ]).


-spec create(dict()) -> binary().
create(Dict) ->
    SessionId = ossp_uuid:make(v4, text),
    Storage = storage(),
    case Storage:put_new(SessionId, Dict) of
        {error, key_already_exists} ->
            create(Dict);
        {ok, SessionId} ->
            SessionId
    end.

-spec delete(binary()) -> true.
delete(SessionId) ->
    Storage = storage(),
    Storage:delete(SessionId).

-spec save(binary(), dict()) -> true.
save(SessionId, Dict) ->
    Storage = storage(),
    Storage:set(SessionId, Dict).

-spec get(binary()) -> expired | not_found | dict().
get(SessionId) ->
    Expiry = expiry(),
    Storage = storage(),
    Storage:get(SessionId, Expiry).

% private
storage() -> 
    {ok, S} = application:get_env(spurs, backend),
    S.

expiry() -> 
    {ok, E} = application:get_env(spurs, expiry),
    E.
