-module(eschat_websocket_h).
-behavior(cowboy_websocket).
-export([websocket_init/1]).
-export([init/2, websocket_handle/2, websocket_info/2, terminate/3]).
-include("eschat_state_h.hrl").

init(#{bindings := #{id := Id} = _Det} = Req, Opts) ->
    {ChatId, _} = string:to_integer(binary_to_list(Id)),
    case eschat_session:validate_session(Req) of
        {ok, {UserId, Login}} ->
            {cowboy_websocket, Req, #state{chatId = ChatId, userId = UserId, nick = Login}};
        {error, _Type} ->
            {ok, cowboy_req:reply(401, Req), undefined}
    end.

websocket_init(State) ->
    eschat_msg:send(#{<<"cmd">> => <<"join">>}, State),
    {ok, State}.
websocket_handle({text, Msg}, State) ->
    JsonMsg = eschat_json:decode(Msg),
    eschat_msg:send(JsonMsg, State);
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {[{text, Msg}], State};
websocket_info({deliver, Message, MsgId}, State) ->
    ChatId = State#state.chatId,
    UserId = State#state.userId,
    case eschat_msg:get_last_rmsg(UserId, ChatId) of
        {ok, LastMsgId} -> eschat_msg:update_last_rmsg(ChatId, UserId, MsgId);
        {error, undefined} -> eschat_msg:create_last_rmsg(ChatId, UserId, MsgId);
        {error, Type} -> {error, Type}
    end,
    {reply, {text, Message}, State};
websocket_info(_Info, State) ->
    {[], State}.

terminate(_Reason, _Req, State) ->
    eschat_msg:send(#{<<"cmd">> => <<"disconnect">>}, State),
    ok.
