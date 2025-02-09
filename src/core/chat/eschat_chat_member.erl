%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2025 5:05 PM
%%%-------------------------------------------------------------------
-module(eschat_chat_member).
-author("ginleaf").

%% API
-export([new/3]).
-export([get/2]).
-export([get/1]).

new(UserId, ChatId, IsOwner) ->
  Res = eschat_pool_manage:db_runner(<<"INSERT into \"ChatMember\" (user_id, chat_id, is_owner) VALUES($1, $2, $3)">>, [UserId, ChatId, IsOwner]),
  case Res of
    {error, Type} -> {error, Type};
    {ok, _} -> {ok, ChatId}
  end.

get(ChatId) ->
  Res = eschat_pool_manage:db_runner(<<"SELECT user_id, is_owner FROM public.\"ChatMember\" WHERE chat_id = $1">>, [ChatId]),
  case Res of
    {error, Type} -> {error, Type};
    {ok, {_, Users}} ->
      {NewUsers, Owner} = make_users_map(Users),
      io:format("Owner ~p~n", [Owner]),
      {ok, NewUsers, Owner}
  end.

get(ChatId, UserId) ->
  {_, FunR} = eschat_pool_manage:db_runner(<<"SELECT is_owner FROM public.\"ChatMember\" WHERE user_id = $1 AND chat_id = $2">>, [UserId, ChatId]),
  io:format("FunR Members: ~p~n", [FunR]),
  case FunR of
    {error, Type} -> {error, Type};
    {ok, _} -> {ok, ChatId}
  end.



make_users_map(T) ->
  Users = lists:map(fun({UserId, IsOwner}) ->
    #{user_id => UserId, is_owner => IsOwner}
  end, T),

  Owner = find_owner(Users),
  {Users, Owner}.

find_owner([#{is_owner := true, user_id := UserId} = _Owner | _]) ->
  UserId;
find_owner([_ | Rest]) ->
  find_owner(Rest);
find_owner([]) ->
  undefined.