-define(NOW_SEC, erlang:system_time(second)).
-define(CACHE_TTL, 10).
-define(MAX_HISTORY, 100).

-record(state, {
  chatId,
  userId,
  nick,
  text,
  ws_pid
}).