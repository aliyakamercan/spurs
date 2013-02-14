-module(spurs_middleware).

-export([execute/2]).

-define(COOKIE_NAME, <<"_spurs_session">>).

execute(Req, Env) ->
    {SessionId, _} = cowboy_req:cookie(?COOKIE_NAME, Req),
    case lists:keyfind(handler, 1, Env) of
        {handler, cowboy_static} -> 
            {ok, Req, Env};
        {handler, _Handler} ->
            case SessionId of
                true -> 
                    create_session(Req, Env);
                undefined -> 
                    create_session(Req, Env);
                SessionId ->
                    case check_session(SessionId, Req) of
                        invalid ->
                            create_session(Req, Env);
                        Dict ->
                            {ok, Req, modify_env(SessionId, Dict, Env)}
                    end
            end
    end.

%internal 

create_session(Req, Env) ->
    {UA, Req2} = cowboy_req:header(<<"user-agent">>, Req),
    {IP, Req3} = cowboy_req:peer_addr(Req2),
    Dict = dict:from_list([{ua, UA}, {ip, IP}]),
    SessionId = spurs_session:create(Dict),
    Req4 = cowboy_req:set_resp_cookie(?COOKIE_NAME, SessionId, options(), Req3),
    {ok, Req4, modify_env(SessionId, Dict, Env)}.

check_session(SessionId, Req) ->
    case spurs_session:get(SessionId) of
        not_found -> invalid;
        expired -> invalid;
        Dict -> 
            {UA, Req2} = cowboy_req:header(<<"user-agent">>, Req),
            {IP, _} = cowboy_req:peer_addr(Req2),
            case {dict:find(ua, Dict), dict:find(ip, Dict)} of
                {{ok, UA}, {ok, IP}} ->
                    Dict;
                _ ->
                    spurs_session:delete(SessionId),
                    invalid
            end
    end.

modify_env(SessionId, Dict, Env) ->
    {handler_opts, HandlerOpts} = lists:keyfind(handler_opts, 1, Env),
    NewOpts = [{session_id, SessionId}, {session, Dict} | HandlerOpts],
    lists:keyreplace(handler_opts, 1, Env, {handler_opts, NewOpts}).

options() ->
    {ok, CO} = application:get_env(spurs, cookie_opts),
    CO. 
