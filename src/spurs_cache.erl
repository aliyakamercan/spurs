-module(spurs_cache).

-author('Ali Yakamercan <aliyakamercan@gmail.com>').

-export([new/0, 
         put_new/2, 
         delete/1, 
         set/2, 
         get/2, 
         destroy/0]).

-type key() :: binary().
-type values() :: dict().

-record(session, {
	id :: key(),
    ts :: {integer(), integer(), integer()},
	values :: values()
}).

-define(SESSION_TABLE, spurs_sessions_table).

new() -> 
	ets:new(?SESSION_TABLE, [set,public, named_table, {keypos, #session.id}]).
	
put_new(Id, Dict) ->
	Time = now(),
	case ets:insert_new(?SESSION_TABLE, #session{id = Id, ts = Time, 
                                              values = Dict}) of
		false ->  
			{error, key_already_exists};
		true -> 
            {ok, Id}
	end.

delete(Id) ->
    ets:delete(?SESSION_TABLE, Id).

%% insert data
set(Id, NewDict) ->
	Time = now(),
    [#session{values = OldDict}] = ets:lookup(?SESSION_TABLE, Id),
    MergedDict = dict:merge(fun(_K, V1, _V2) ->
                    V1
            end, NewDict, OldDict),
    ets:update_element(?SESSION_TABLE, Id, [{#session.ts, Time},
                                            {#session.values, MergedDict}]).


get(Id, Expiry) ->
	case ets:lookup(?SESSION_TABLE, Id) of
		[#session{values= Dict, ts = Time}] ->
            case timer:now_diff(now(), Time)/1000000 > Expiry of
                true ->
                    ets:delete(?SESSION_TABLE, Id),
                    expired;
                false ->
                    Dict
            end;
		[] ->
			not_found
	end.

destroy() ->
    ets:delete(?SESSION_TABLE).

