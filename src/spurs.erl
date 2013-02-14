-module(spurs).

-export([start/0, 
         save/1,
         set/3,
         delete/1,
         get_n/2,
         set_p/3,
         get/2]).

-type state() :: [{any(), any()}].

-spec start() -> ok.
start() ->
    application:start(spurs).

% set value
-spec set(state(), any(), any()) -> state().
set(State, Key, Value) ->
    {_, Session} = lists:keyfind(session, 1, State),
    Session1 = dict:store(Key, Value, Session),
    lists:keyreplace(session, 1, State, {session, Session1}).

% set and write 
-spec set_p(state(), any(), any()) -> state().
set_p(State, Key, Value) ->
    State2 = set(State, Key, Value), 
    save(State2),
    State2.

% get value from state
-spec get(state(), any()) -> undefined | any.
get(State, Key) ->
    {_, Session} = lists:keyfind(session, 1, State),
    case dict:find(Key, Session) of
        error -> undefined;
        {ok, Value} -> Value
    end.

% gets the value from backend - not the state
-spec get_n(state(), any()) -> not_found | undefined | any().
get_n(State, Key) ->
    {_, SessionId} = lists:keyfind(session_id, 1, State),
    case spurs_session:get(SessionId) of
        expired -> not_found;
        not_found -> not_found;
        Dict ->
            case dict:find(Key, Dict) of
                error -> undefined;
                {ok, Value} ->
                    Value
            end
    end.

% write all the changes
-spec save(state()) -> true.
save(State) ->
    {_, SessionId} = lists:keyfind(session_id, 1, State),
    {_, Session} = lists:keyfind(session, 1, State),
    spurs_session:save(SessionId, Session).

% delete the session
-spec delete(state()) -> state().
delete(State) ->
    {_, SessionId} = lists:keyfind(session_id, 1, State),
    spurs_session:delete(SessionId),
    proplists:delete(session, proplists:delete(session_id, State)).

