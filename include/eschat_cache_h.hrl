-define(NOW_SEC, erlang:system_time(second)).
-define(CACHE_TTL, 10).
-define(PROC_LIST, [eschat_user:name(), eschat_session:name()]).
% -define(TTL, ?CACHE_TTL + ?NOW_SEC)

-record(cache, {
  id,
  values,
  ttl = (?NOW_SEC + ?CACHE_TTL)
}).
