-module(eschat_message_h).
-author("ginleaf").
-behavior(cowboy_handler).

%% API
-export([init/2]).

-include("eschat_msg_h.hrl").

init(Req, Env) ->
    case eschat_session:validate_session(Req) of
        {ok, {UserId, _}} -> init(Req, Env, UserId);
        {error, Type} -> eschat_h:build_res(Req, #{<<"details">> => Type})
    end.
init(#{method := <<"GET">>, bindings := #{id := Id} = _Det} = Req, _Env, _UserId) ->
    {MsgId, _} = string:to_integer(binary_to_list(Id)),
    case eschat_msg:get_m(MsgId) of
        {Status, #msg{id = MsgId,chatId = ChatId,updated = Updated, created = Created, text = Text, userId = UserId}} ->
            eschat_h:build_res(Req, #{
                <<"user_id">> => UserId,  <<"text">> => Text, <<"message_id">> => MsgId, <<"chat_id">> => ChatId, <<"updated_tz">> => Updated, <<"created_tz">> => Created
            },#{<<"x-cache">> => erlang:atom_to_binary(Status)});
        {error, Type} ->
            eschat_h:build_res(Req, #{<<"details">> => Type})
    end;
init(#{method := <<"DELETE">>, bindings := #{id := Id} = _Det} = Req, _Env, UserId) ->
        {MsgId, _} = string:to_integer(binary_to_list(Id)),
        {_Status, Type} = eschat_msg:remove(MsgId, UserId),
        eschat_h:build_res(Req, #{<<"details">> => Type});

init(#{method := <<"PUT">>, bindings := #{id := Id} = _Det} = Req, _Env, UserId) ->
        {MsgId, _} = string:to_integer(binary_to_list(Id)),
        case eschat_h:req_decode(Req) of
            {ok, Data, Req2} ->
                {_Status, Type} = eschat_msg:change_m(MsgId, UserId, Data),
                eschat_h:build_res(Req2, #{<<"details">> => Type});
                % cowboy_req:reply(200, Req3);
            {error, Reason} ->
                eschat_h:build_res(Req, #{<<"details">> => Reason})
        end.
