%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2024 9:58 AM
%%%-------------------------------------------------------------------
-module(eschat_user_h).
-behavior(cowboy_handler).

-include("eschat_user_h.hrl").
-include("eschat_session_h.hrl").

-define(RESP_HEADERS(LINE, STATUS), #{
    <<"status">> => STATUS,
    <<"content-type">> => <<"application/json">>,
    <<"x-api">> => erlang:atom_to_binary(?MODULE),
    <<"x-init">> => LINE
}).

%% API
-export([init/2]).

init(#{method := <<"GET">>, bindings := #{action := <<"session">>} = Det} = Req, _Env) ->
    SessionId = maps:get(id, Det),
    SessInfo = eschat_session:get_sess(SessionId),
    % Req2 =
        case SessInfo of
            {error, Type} ->
                eschat_h:build_res(Req, #{<<"details">> => Type});
            {Status, {_, _, UserId, Time, _}} ->
                eschat_h:build_res(Req, #{<<"userId">> => UserId, <<"Time">> => Time}, #{<<"x-cache">> => erlang:atom_to_binary(Status)})
        end;
    % Res = cowboy_req:reply(200, Req2),
    % {ok, Res, Env};
init(Req, Env) ->
    % Res =
        case eschat_h:req_decode(Req) of
            {ok, Data, Req2} ->
                init(Req, Env, {Req2, Data});
                % cowboy_req:reply(200, Req3);
            {error, Reason} ->
                eschat_h:build_res(Req, #{<<"details">> => Reason})
        end.
    % {ok, Res, Env}.
init(#{method := <<"GET">>, bindings := #{action := <<"user">>} = _Det} = _, _Env, {Req, Data}) ->
    case eschat_user:get_user(Data) of
        {error, Type} ->
            eschat_h:build_res(Req, #{<<"details">> => Type});
        {Status, UserData} ->
            eschat_h:build_res(Req, #{<<"details">> => UserData}, #{
                <<"x-cache">> => erlang:atom_to_binary(Status)
            })
    end;
init(#{method := <<"POST">>, bindings := #{action := <<"login">>} = _Det} = _, _Env, {Req, Data}) ->
    case eschat_user:get_user(Data) of
        {error, Type} ->
            eschat_h:build_res(Req, #{<<"details">> => Type});
        {Status, UserData} ->
            session_create(UserData, Status, Req)
    end;
init(#{method := <<"POST">>, bindings := #{action := <<"reg">>} = _Det} = _, _Env, {Req, Data}) ->
    case eschat_user:new_user(Data) of
        {error, Type} ->
            eschat_h:build_res(Req, #{<<"details">> => Type});
        {Status, _} ->
            eschat_h:build_res(
                Req,
                #{<<"details">> => <<"User created">>},
                #{<<"x-cache">> => erlang:atom_to_binary(Status)}
            )
    end;

init(Req, Env, _) ->
    eschat_notfound_h:init(cowboy_req:set_resp_headers(?RESP_HEADERS(<<"3">>, <<"ok">>), Req), Env).

session_create(UserId, Status, Req) ->
    case eschat_session:new_sess(UserId) of
        {error, _Type} ->
            error;
        {StatusSess, #session{id = SessionId}} ->
            eschat_h:build_res(
                Req,
                #{
                    <<"details">> => UserId,
                    <<"sessionId">> => SessionId
                },
                #{<<"x-cache">> => erlang:atom_to_binary(Status),
                <<"x-sess-cache">> => erlang:atom_to_binary(StatusSess)}
            )
    end.


