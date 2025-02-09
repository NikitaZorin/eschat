-define(NOW_SEC, erlang:system_time(second)).
-define(CACHE_TTL, 10).

-record(user, {
  id,
  login,
  password,
  ttl = (?NOW_SEC + ?CACHE_TTL)
}).