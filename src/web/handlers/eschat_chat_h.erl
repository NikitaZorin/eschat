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

init(#{method := <<"GET">>, bindings := #{id:= Id} = _Det} = Req, Env) ->
  {ChatId, _} = string:to_integer(binary_to_list(Id)),
  lager:debug("NewChatId ~p~n", [ChatId]),
  Result = case eschat_chat:get(ChatId) of
          {ok, Users} ->
            eschat_user_h:build_res(Req, #{<<"chat_id">> => Id, <<"users">> => Users});
             {error, Type} -> eschat_user_h:build_res(Req, #{<<"details">> => Type})
        end,
  Res = cowboy_req:reply(200, Result),
  {ok, Res, Env};

init(#{method := <<"PUT">>, bindings := #{id := Id} = _Det} = Req, Env) ->
  {ChatId, _} = string:to_integer(binary_to_list(Id)),
  UserId = 44,
  Req1 = eschat_user_h:req_decode(Req),
  Req2 = case Req1 of
    {ok, _Data, Req3} ->
%%      case eschat_chat_member:get(Id, UserId) of
      case eschat_chat:get(ChatId) of
        {error, Type} -> eschat_user_h:build_res(Req, #{<<"details">> => Type});
        {ok, Users , OwnerId} -> eschat_user_h:build_res(Req3, #{<<"users">> => Users, <<"owner_id">> => OwnerId})
      end
  end,

  Res = cowboy_req:reply(200, Req2),
  {ok, Res, Env};
init(#{method := <<"POST">>} = Req, Env) ->
  Req1 = eschat_user_h:req_decode(Req),
  Req2 = case Req1 of
           {ok, Data, Req3} ->
             case eschat_chat:new(Data) of
               {error, Type} ->
                 eschat_user_h:build_res(Req3, #{<<"details">> => Type});
               {_, ChatData} ->
                 eschat_user_h:build_res(Req3, #{<<"details">> => <<"Chat created">>, <<"chat_id">> => ChatData})
             end;
               _ -> Req
         end,

 Res = cowboy_req:reply(200, Req2),
  {ok, Res, Env};





init(Req, Env) ->
  eschat_notfound_h:init(cowboy_req:set_resp_headers(?RESP_HEADERS(<<"3">>, <<"ok">>), Req), Env).

