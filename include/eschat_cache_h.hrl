-define(NOW_SEC, erlang:system_time(second)).
-define(CACHE_TTL, 30).

-record(cache, {
  id,
  values,
  ttl = (?NOW_SEC + ?CACHE_TTL)
}).
