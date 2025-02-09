%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2025 4:51 PM
%%%-------------------------------------------------------------------
-module(eschat_chat).
-author("ginleaf").

%% API
-export([new/1]).
-export([get/1]).

%%update(Struct) ->
%%

get(ChatId) ->
  DbRes = eschat_pool_manage:db_runner(<<"SELECT id FROM public.\"Chat\" WHERE id = $1">>, [ChatId]),
  case DbRes of
    {error, _} -> {error, chat_not_exist};
    {ok, _} ->
      eschat_chat_member:get(ChatId)
  end.

new(Struct) ->
  case eschat_user:get_user(Struct) of
    {error, undefined} ->
      {error, user_not_found};
    {error, Type} ->
      {error, Type};
    {_, UserData}  ->
      ChatName = eschat_xpath:get_val(<<"chat_name">>, Struct),
      Res = eschat_pool_manage:db_runner(<<"INSERT into \"Chat\" (name) VALUES($1) Returning id">>, [ChatName]),
      case Res of
        {error, Type} ->
          {error, Type};
        {_, {ok, [{ChatData } | _]}} ->
          Users = eschat_xpath:get_val(<<"users">>, Struct),
          add_users_to_chat(Users, UserData, ChatData)
      end
  end.


add_users_to_chat(Users, UserId, ChatId) ->
  SqlUsers = eschat_pool_manage:db_runner(<<"SELECT * FROM \"User\" WHERE login = ANY($1)">>, [Users]),
  lager:debug("SqlUsers ~p", [SqlUsers]),
  case SqlUsers of
    {ok, {ok, Members}} -> bulk_members(Members, ChatId);
    _ -> {error, db_error}
  end,
  eschat_chat_member:new(UserId, ChatId, true).

bulk_members([], _) ->
  {result, ok};
bulk_members([{Id, _, _} | Rest], ChatId) ->
  eschat_chat_member:new(Id, ChatId, false),
  bulk_members(Rest, ChatId).



