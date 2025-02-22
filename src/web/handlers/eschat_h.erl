%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2025 5:26 PM
%%%-------------------------------------------------------------------
-module(eschat_h).
-author("ginleaf").

%% API
-export([req_decode/1]).
-export([build_res/2]).
-export([build_res/3]).

-define(RESP_HEADERS(LINE, STATUS), #{
  <<"status">> => STATUS,
  <<"content-type">> => <<"application/json">>,
  <<"x-api">> => erlang:atom_to_binary(?MODULE),
  <<"x-init">> => LINE
}).

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
  Result = cowboy_req:set_resp_body(JSB, Headers),
  Res = cowboy_req:reply(200, Result),
  Env = [],
  {ok, Res, Env}.