% -define(CACHE_TTL, 600).
-define(CACHE_TTL_MS, 6000).

-record(session, {
  id,
  userId,
  activeTo,
  login
}).
