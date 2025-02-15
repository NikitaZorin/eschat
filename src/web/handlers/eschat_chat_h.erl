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

-define(RESP_HEADERS(LINE, STATUS), #{
    <<"status">> => STATUS,
    <<"content-type">> => <<"application/json">>,
    <<"x-api">> => erlang:atom_to_binary(?MODULE),
    <<"x-init">> => LINE
}).

% get(ChatId, UserId) ->
%   {_, FunR} = eschat_pool_manage:db_runner(<<"SELECT is_owner FROM public.\"ChatMember\" WHERE user_id = $1 AND chat_id = $2">>, [UserId, ChatId]),
%   case FunR of
%     {error, Type} -> {error, Type};
%     {ok, _} -> {ok, ChatId}
%   end.

init(#{method := <<"DELETE">>, bindings := #{id := Id} = _Det} = Req, Env) ->
    {ChatId, _} = string:to_integer(binary_to_list(Id)),
    UserId = 44,

    {_Status, Type} = eschat_chat:remove(ChatId, UserId),
    Result = eschat_h:build_res(Req, #{<<"details">> => Type}),

    Res = cowboy_req:reply(200, Result),
    {ok, Res, Env};
init(#{method := <<"GET">>, bindings := #{id := Id} = _Det} = Req, Env) ->
    {ChatId, _} = string:to_integer(binary_to_list(Id)),
    lager:debug("NewChatId ~p~n", [ChatId]),
    Result =
        case eschat_chat:get(ChatId) of
            {ok, Users, OwnerId} ->
                eschat_h:build_res(Req, #{
                    <<"chat_id">> => ChatId, <<"users">> => Users, <<"owner_id">> => OwnerId
                });
            {error, Type} ->
                eschat_h:build_res(Req, #{<<"details">> => Type})
        end,
    Res = cowboy_req:reply(200, Result),
    {ok, Res, Env};
init(#{method := <<"PUT">>, bindings := #{id := Id} = _Det} = Req, Env) ->
    {ChatId, _} = string:to_integer(binary_to_list(Id)),
    Req1 = eschat_h:req_decode(Req),
    Req2 =
        case Req1 of
            {ok, Data, Req3} ->
                UserId = eschat_xpath:get_val(<<"user_id">>, Data),
                % remove get from req data and replace this by checking session ID
                io:format("UserId ~p~n", [UserId]),
                case eschat_chat:get(ChatId) of
                    {error, Type} ->
                        eschat_h:build_res(Req, #{<<"details">> => Type});
                    {ok, _Users, _UserId} ->
                        UpdateRes = eschat_chat:update(Data, ChatId),
                        lager:debug("Test UPDATE ~p~n", [UpdateRes]),
                        eschat_h:build_res(Req3, #{<<"details">> => chat_updated});
                    {ok, Users, _} ->
                        eschat_h:build_res(Req, #{<<"details">> => you_not_owner})
                end
        end,
    Res = cowboy_req:reply(200, Req2),
    {ok, Res, Env};
init(#{method := <<"POST">>} = Req, Env) ->
    Req1 = eschat_h:req_decode(Req),
    Req2 =
        case Req1 of
            {ok, Data, Req3} ->
                case eschat_chat:new(Data) of
                    {error, Type} ->
                        eschat_h:build_res(Req3, #{<<"details">> => Type});
                    {_, ChatData} ->
                        eschat_h:build_res(Req3, #{
                            <<"details">> => <<"Chat created">>, <<"chat_id">> => ChatData
                        })
                end;
            _ ->
                Req
        end,

    Res = cowboy_req:reply(200, Req2),
    {ok, Res, Env};
init(Req, Env) ->
    eschat_notfound_h:init(cowboy_req:set_resp_headers(?RESP_HEADERS(<<"3">>, <<"ok">>), Req), Env).
