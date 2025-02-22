-module(eschat_cache).

-include("eschat_cache_h.hrl").

-export([new/1]).
-export([nowe/0]).
-export([to_cache/3]).
-export([from_cache/2]).
-export([traverse_fun/1]).



new(Name) ->
  ets:new(Name, [named_table, set, public, {keypos, #cache.id}]).

to_cache(Rec, TableName, Id) ->
  Ttl = ttl(),
  UpdatedRec = #cache{ttl = Ttl, values = Rec, id = Id},
  ets:insert(TableName, UpdatedRec).

from_cache(TableName, Key) ->
  Test = ets:lookup(TableName, Key),
  Now = nowe(),
  Res = case Test of
    [#cache{ttl = StoredNow, values = Values} = _Rec] when StoredNow >= Now -> Values;
    _ -> {error, undefined}
  end,
  Res.

nowe() -> ?NOW_SEC.



ttl() -> ?CACHE_TTL + ?NOW_SEC.

traverse_fun(TableName) ->
  fun (Key, Now) ->
    case ets:lookup(TableName, Key) of
      [#cache{ttl = StoredNow}] when StoredNow >= Now -> ok;
      [#cache{}] -> ets:delete(TableName, Key), ok;
      [] -> ok
    end
  end.

