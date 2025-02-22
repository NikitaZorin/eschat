%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 10:14 AM
%%%-------------------------------------------------------------------
-module(eschat_chat_h).
-author("ginleaf").
-behavior(cowboy_handler).

%% API
-export([init/2]).

-include("eschat_chat_h.hrl").

-define(RESP_HEADERS(LINE, STATUS), #{
    <<"status">> => STATUS,
    <<"content-type">> => <<"application/json">>,
    <<"x-api">> => erlang:atom_to_binary(?MODULE),
    <<"x-init">> => LINE
}).


init(Req, Env) ->
    case eschat_session:validate_session(Req) of
            {ok, {UserId, _}} -> init(Req, Env, UserId);
            % add case of init
            {error, Type} ->  eschat_h:build_res(Req, #{<<"details">> => Type})
    end.
init(#{method := <<"GET">>, bindings := #{id := Id} = _Det} = Req, _Env, _UserId) ->
    {ChatId, _} = string:to_integer(binary_to_list(Id)),
    case eschat_chat:get(ChatId) of
            {Status, #chat{id = ChatId,name = Name,users = Users, owner = OwnerId}} ->
                eschat_h:build_res(Req, #{
                    <<"chat_name">> => Name, <<"chat_id">> => ChatId, <<"users">> => Users, <<"owner_id">> => OwnerId
                },#{<<"x-cache">> => erlang:atom_to_binary(Status)});
            {error, Type} ->
                eschat_h:build_res(Req, #{<<"details">> => Type})
    end;
init(#{method := <<"DELETE">>, bindings := #{id := Id} = _Det} = Req, _Env, UserId) ->
    {ChatId, _} = string:to_integer(binary_to_list(Id)),
    {_Status, Type} = eschat_chat:remove(ChatId, UserId),
    eschat_h:build_res(Req, #{<<"details">> => Type});
init(#{method := <<"PUT">>, bindings := #{id := Id} = _Det} = Req, _Env, UserId) ->
    {ChatId, _} = string:to_integer(binary_to_list(Id)),
    Req1 = eschat_h:req_decode(Req),
    case Req1 of
            {ok, Data, Req3} ->
                case eschat_chat:get(ChatId) of
                    {error, Type} ->
                        eschat_h:build_res(Req, #{<<"details">> => Type});
                    {Status, #chat{owner = UserId} = _Rec} ->
                        _UpdateRes = eschat_chat:update(Data, ChatId),
                        eschat_h:build_res(Req3, #{<<"details">> => chat_updated}, #{<<"x-cache">> => erlang:atom_to_binary(Status)});
                    {Status, _Rec} ->
                        eschat_h:build_res(Req, #{<<"details">> => you_not_owner}, #{<<"x-cache">> => erlang:atom_to_binary(Status)})
                end
        end;
init(#{method := <<"POST">>} = Req, _Env, UserId) ->
    Req1 = eschat_h:req_decode(Req),
    case Req1 of
            {ok, Data, Req3} ->
                case eschat_chat:new(Data, UserId) of
                    {error, Type} ->
                        eschat_h:build_res(Req3, #{<<"details">> => Type});
                    {_, ChatData} ->
                        eschat_h:build_res(Req3, #{
                            <<"details">> => <<"Chat created">>, <<"chat_id">> => ChatData
                        })
                end;
            _ ->
                Req
        end;
init(Req, Env, _) ->
    eschat_notfound_h:init(cowboy_req:set_resp_headers(?RESP_HEADERS(<<"3">>, <<"ok">>), Req), Env).
