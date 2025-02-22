-module(eschat_msg).
-author("ginleaf").

-export([name/0]).
-export([get_m/1]).
-export([send/2]).
-export([remove/2]).
-export([change_m/3]).
-export([create_last_rmsg/3]).
-export([get_last_rmsg/2]).
-export([update_last_rmsg/3]).



-include("eschat_state_h.hrl").
-include("eschat_msg_h.hrl").


send(#{<<"cmd">> := <<"join">>} = Req, State) ->
    Nick = State#state.nick,
    UserId = State#state.userId,
    eschat_node_session_sup:start_user_session(UserId, self()),
    NewMsg = <<">>> ", Nick/binary, " join chat!">>,
    send_msg(NewMsg, State, <<"join">>);
send(#{<<"cmd">> := <<"disconnect">>} = Req, State) ->
    Nick = State#state.nick,
    UserId = State#state.userId,
    eschat_node_session:stop_session(UserId),
    NewMsg = <<">>> ", Nick/binary, " leaves the chat! Goodbye!">>,
    send_msg(NewMsg, State, <<"disconnect">>);
% send(#{<<"cmd">> := <<"remove">>} = Req, State) ->
% send(#{<<"cmd">> := <<"edit">>} = Req, State) ->
%       Nick = State#state.nick,
%       UserId = State#state.userId,
%       eschat_node_session:stop_session(UserId),
%       NewMsg = <<">>> ", Nick/binary, " leaves the chat! Goodbye!">>,
%       send_msg(NewMsg, State, <<"disconnect">>);
send(#{<<"cmd">> := <<"send_message">>, <<"msg">> := Msg} = Req, State) ->
    send_msg(Msg, State, <<"send_message">>).

get_m(MsgId) ->
    case eschat_cache:from_cache(name(), MsgId) of
        #msg{id = MsgId} = Rec ->
            {hit, Rec};
        _ ->
            {_, FunR} = eschat_pool_manage:db_runner(
                <<"SELECT * FROM public.\"ChatMessage\" WHERE id = $1">>, [MsgId]
            ),
            case FunR of
                {ok, [{Id, ChatId, Text, ReplyF, UserId, Created, Updated} | _]} ->
                    Rec = #msg{
                        id = Id,
                        chatId = ChatId,
                        text = Text,
                        updated = Updated,
                        created = Created,
                        userId = UserId
                    },
                    eschat_cache:to_cache(Rec, name(), Id),
                    % to_cache(UserRec),
                    {miss, Rec};
                {ok, []} ->
                    {error, undefined};
                _ ->
                    {error, db_error}
            end
    end.

change_m(MsgId, UserId, Struct) ->
  Res = get_m(MsgId),
  case Res of
      {error, Type} ->
          {error, Type};
      {_Status, #msg{userId = UserId}}  ->
          NewText = eschat_xpath:get_val(<<"text">>, Struct),
          DbRes = eschat_pool_manage:db_runner(
              <<"UPDATE \"ChatMessage\" SET message = $1, edited_at = $2 WHERE author_id = $3">>, [NewText, erlang:timestamp(), UserId]
          ),

          case DbRes of
              {ok, _} -> {ok, msg_updated};
              _ -> {error, db_error}
          end;
      _ ->
          {error, not_have_permissions}
  end. 

% check_time({Date, {Hour, Minute, SecondFloat}}) ->
%     MsgTime = calendar:datetime_to_gregorian_seconds({Date, {Hour, Minute, trunc(SecondFloat)}}),
%     NowTime = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
%     NowTime - MsgTime >= 600.

remove(MsgId, UserId) ->
    Res = get_m(MsgId),
    case Res of
        {error, Type} ->
            {error, Type};
        {_Status, #msg{userId = UserId}} ->
            DbRes = eschat_pool_manage:db_runner(
                <<"DELETE from \"ChatMessage\" WHERE id = $1">>, [MsgId]
            ),
            case DbRes of
                {ok, _} -> {ok, msg_deleted};
                _ -> {error, db_error}
            end;
        _ ->
            {error, not_have_permissions}
    end.

send_msg(Msg, State, Action) ->
    Timestamp = get_time(),
    Nick = State#state.nick,
    ChatId = State#state.chatId,
    UserId = State#state.userId,
    case eschat_chat:get(ChatId) of
        {_Status, {_, _Id, _Name, Users, _}} ->
            Res = save_msg(Msg, State),
            Data =
                case Res of
                    {error, Type} ->
                        #{status => <<"error">>, details => Type};
                    {_, {ok, [{MsgId} | _]}} ->
                        #{
                            action => Action,
                            status => <<"ok">>,
                            message => Msg,
                            messageId => MsgId,
                            time => Timestamp,
                            user => Nick,
                            userId => UserId
                        }
                end,
            JsonEn = eschat_json:encode(Data),
            send_to_chat(Users, JsonEn, UserId, maps:get(messageId, Data)),
            {[{text, JsonEn}], State, hibernate};
        {error, Type} ->
            {error, Type}
    end.

name() ->
    ?MODULE.

send_to_chat(Users, Msg, SenderUserId, MsgId) ->
    lists:foreach(
        fun(User) ->
            UserId = maps:get(user_id, User),
            if
                UserId =:= SenderUserId ->
                    ok;
                true ->
                    case eschat_node_session:send_message(UserId, Msg, MsgId) of
                        ok ->
                            ok;
                        {error, user_not_found} ->
                          {error, user_not_found}
                    end
            end
        end,
        Users
    ).

% get_msg()

save_msg(Msg, State) ->
    ChatId = State#state.chatId,
    UserId = State#state.userId,
    ReplyFor = undefined,
    eschat_pool_manage:db_runner(
        <<"INSERT into \"ChatMessage\" (chat_id, message, reply_for, author_id, created_at) VALUES($1, $2, $3, $4, $5) Returning id">>,
        [ChatId, Msg, ReplyFor, UserId, erlang:timestamp()]
    ).

create_last_rmsg(ChatId, UserId, MsgId) ->
    Res = eschat_pool_manage:db_runner(
        <<"INSERT into \"LastReadMsgInChat\" (chat_id, user_id, chat_message_id) VALUES($1, $2, $3)">>,
        [ChatId, UserId, MsgId]
    ),
    Res.

update_last_rmsg(ChatId, UserId, MsgId) ->
        Res = eschat_pool_manage:db_runner(
            <<"UPDATE \"LastReadMsgInChat\" SET chat_message_id = $1 WHERE user_id = $2 and chat_id = $3">>,
            [MsgId, UserId, ChatId]
        ),
        Res.

get_last_rmsg(UserId, ChatId) ->
    {_, FunR} = eschat_pool_manage:db_runner(
        <<"SELECT chat_message_id FROM public.\"LastReadMsgInChat\" WHERE user_id = $1 and chat_id = $2">>,
        [UserId, ChatId]
    ),
    case FunR of
        {ok, [{ChatMessageId}]} ->
            {ok, ChatMessageId};
        {ok, []} ->
            {error, undefined};
        _ ->
            {error, db_error}
    end.

get_time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
    TimeString = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [
        Year, Month, Day, Hour, Minute, Second
    ]),
    erlang:iolist_to_binary(TimeString).
