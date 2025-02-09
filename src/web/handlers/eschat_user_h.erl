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

-define(RESP_HEADERS(LINE, STATUS), #{
  <<"status">> => STATUS,
  <<"content-type">> => <<"application/json">>,
  <<"x-api">> => erlang:atom_to_binary(?MODULE),
  <<"x-init">> => LINE
}).

%% API
-export([init/2]).
-export([req_decode/1]).
-export([build_res/2]).
-export([build_res/3]).


init(#{method := <<"GET">>, bindings := #{action:= <<"session">>} = Det} = Req, Env) ->
  SessionId = maps:get(id, Det),
  {_, {_, _, UserId, Time}} = eschat_session:get_sess(SessionId),
  Req2 = build_res(Req, #{<<"userId">> => UserId, <<"Time">> => Time}),
  Res = cowboy_req:reply(200, Req2),
  {ok, Res, Env};

init(#{method := <<"POST">>, bindings := #{action:= <<"login">>} = _Det} = Req, Env) ->
  Req1 = req_decode(Req),
  Req2 = case Req1 of
           {ok, Data, Req3} ->
             case eschat_user:get_user(Data) of
               {error, Type} ->
                 build_res(Req3, #{<<"details">> => Type});
               {Status, UserData} ->
                 case eschat_session:new_sess(UserData) of
                   {error, _Type} -> error;
                   {ok, SessionId} ->
                     build_res(Req3, #{<<"details">> => UserData, <<"sessionId">> => list_to_binary(SessionId)},
                       #{<<"x-cache">> => erlang:atom_to_binary(Status)})
                 end
             end;
           _ -> Req
         end,
  Res = cowboy_req:reply(200, Req2),
  {ok, Res, Env};

init(#{method := <<"POST">>, bindings := #{action:= <<"reg">>} = _Det} = Req, Env) ->
  Req1 = req_decode(Req),
  Req2 = case Req1 of
           {ok, Data, Req3} ->
             case eschat_user:new_user(Data) of
               {error, Type} ->
                 build_res(Req3, #{<<"details">> => Type});
               {Status, _} ->
                 build_res(Req3, #{<<"details">> => <<"User created">>},
                   #{<<"x-cache">> => erlang:atom_to_binary(Status)})
             end;
             _ -> Req
         end,
  Res = cowboy_req:reply(200, Req2),
  {ok, Res, Env};

init(Req, Env) ->
  eschat_notfound_h:init(cowboy_req:set_resp_headers(?RESP_HEADERS(<<"3">>, <<"ok">>), Req), Env).

req_decode(Req) ->
  CT = cowboy_req:header(<<"content-type">>, Req),
  lager:debug("CT ~p", [CT]),
  Result = case CT of
             <<"application/json">>    ->
               {ok, Body, Req2} = eschat_http_body:read(Req),
               {ok, eschat_json:decode(Body), Req2};
             <<"application/x-www-form-urlencoded">> ->
               {ok, _Body, _Req2} = cowboy_req:read_urlencoded_body(Req);
             _ -> {error, unxepected_format}
           end,
  Result.



build_res(Req, Body) ->
  build_res_final(Req, Body, ?RESP_HEADERS(<<"1">>, <<"ok">>)).

build_res(Req, Body, Header) ->
  FinalHeaders = maps:merge(?RESP_HEADERS(<<"1">>, <<"ok">>), Header),
  build_res_final(Req, Body, FinalHeaders).

build_res_final(Req, Body, Header) ->
  Headers = cowboy_req:set_resp_headers(Header, Req),
  JSB = eschat_json:encode(Body),
  cowboy_req:set_resp_body(JSB, Headers).
