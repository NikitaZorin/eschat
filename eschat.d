# Generated by Erlang.mk. Edit at your own risk!

src/core/chat/eschat_chat.erl:: include/eschat_chat_h.hrl; @touch $@
src/core/message/eschat_msg.erl:: include/eschat_msg_h.hrl include/eschat_state_h.hrl; @touch $@
src/core/sessions/eschat_session.erl:: include/eschat_session_h.hrl; @touch $@
src/core/users/eschat_user.erl:: include/eschat_user_h.hrl; @touch $@
src/database/eschat_db_sup.erl:: include/eschat_user_h.hrl; @touch $@
src/utils/eschat_cache.erl:: include/eschat_cache_h.hrl; @touch $@
src/web/handlers/eschat_chat_h.erl:: include/eschat_chat_h.hrl; @touch $@
src/web/handlers/eschat_message_h.erl:: include/eschat_msg_h.hrl; @touch $@
src/web/handlers/eschat_notfound_h.erl:: /Users/ginleaf/Documents/Erlang/eschat/deps/epgsql/include/epgsql.hrl; @touch $@
src/web/handlers/eschat_user_h.erl:: include/eschat_session_h.hrl include/eschat_user_h.hrl; @touch $@
src/web/handlers/eschat_websocket_h.erl:: include/eschat_state_h.hrl; @touch $@

COMPILE_FIRST +=
