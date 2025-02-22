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

-include("eschat_chat_h.hrl").

%% API
-export([new/2]).
-export([get/1]).
-export([update/2]).
-export([remove/2]).
-export([name/0]).

get(ChatId) ->
    Sql = <<"SELECT id, name FROM public.\"Chat\" WHERE id = $1">>,
    Res = eschat_cache:from_cache(name(), ChatId),
    case Res of
        #chat{id = ChatId} = Rec ->
            {hit, Rec};
        _ -> to_cache(Sql, ChatId)
    end.
new(Struct, UserId) ->
            ChatName = eschat_xpath:get_val(<<"chat_name">>, Struct),
            Res = eschat_pool_manage:db_runner(
                <<"INSERT into \"Chat\" (name) VALUES($1) Returning id">>, [ChatName]
            ),
            case Res of
                {error, Type} ->
                    {error, Type};
                {_, {ok, [{ChatData} | _]}} ->
                    Users = eschat_xpath:get_val(<<"users">>, Struct),
                    bulk_users_to_chat(Users, ChatData, UserId, add)
            end.
    % end.

remove(ChatId, UserId) ->
    case eschat_chat_member:get(ChatId, UserId) of
        {ok, [{false}]} ->
            {error, you_not_owner};
        {ok, [{true}]} ->
            DbRes = eschat_pool_manage:db_runner(
                <<"DELETE from \"Chat\" WHERE id = $1">>, [ChatId]
            ),
            case DbRes of
                {ok, _} -> {ok, chat_deleted};
                _ -> {error, db_error}
            end;
        {ok, []} ->
            {error, chat_not_exist};
        {error, Type} ->
            {error, Type}
    end.

update(Struct, ChatId) ->
    NewChatName = eschat_xpath:get_val(<<"chat_name">>, Struct),
    ResChangeName = update_name(ChatId, NewChatName),
    ResUsers = update_users(ChatId, Struct),
    {ResChangeName, ResUsers}.

update_users(ChatId, Struct) ->
    NewUsers = eschat_xpath:get_val(<<"new_users">>, Struct),
    RemoveUsers = eschat_xpath:get_val(<<"remove_users">>, Struct),
    {update_users_final(NewUsers, ChatId, add), update_users_final(RemoveUsers, ChatId, remove)}.

update_users_final(Users, ChatId, Action) ->
    case Users of
        [] -> {error, empty_array};
        undefined -> undefined;
        _ -> bulk_users_to_chat(Users, ChatId, Action)
    end.

update_name(ChatId, NewChatName) ->
    case NewChatName of
        undefined ->
            undefined;
        _ ->
            DbRes = eschat_pool_manage:db_runner(
                <<"UPDATE \"Chat\" SET name = $1 WHERE id = $2">>, [NewChatName, ChatId]
            ),
            case DbRes of
                {ok, _} -> {ok, chat_name_updated};
                _ -> {error, db_error}
            end
    end.

bulk_users_to_chat(Users, ChatId, UserId, Action) ->
    eschat_chat_member:new(UserId, ChatId, true),
    bulk_users_to_chat(Users, ChatId, Action).
bulk_users_to_chat(Users, ChatId, Action) ->
    SqlUsers = eschat_pool_manage:db_runner(<<"SELECT * FROM \"User\" WHERE login = ANY($1)">>, [
        Users
    ]),
    lager:debug("SqlUsers ~p", [SqlUsers]),
    case SqlUsers of
        {ok, {ok, Members}} -> bulk_members(Members, ChatId, Action);
        _ -> {error, db_error}
    end.


bulk_members([], ChatId, _) ->
    {result, ChatId};
bulk_members([{Id, _, _} | Rest], ChatId, remove) ->
    eschat_chat_member:remove(Id, ChatId),
    bulk_members(Rest, ChatId, remove);
bulk_members([{Id, _, _} | Rest], ChatId, add) ->
    eschat_chat_member:new(Id, ChatId, false),
    bulk_members(Rest, ChatId, add).

name() ->
    ?MODULE.

to_cache(Sql, ChatId) ->
    {_, FunR} = eschat_pool_manage:db_runner(Sql, [ChatId]),
    case FunR of
        {ok, [{ChatId, Name } | _]} ->
            case eschat_chat_member:get(ChatId) of
                {error, Type} -> {error, Type};
                {ok, Users, Owner} -> 
                    ChatRec = #chat{id = ChatId, name = Name, users = Users, owner = Owner},
                    eschat_cache:to_cache(ChatRec, name(), ChatId),
                    {miss, ChatRec}
            end;
        {ok, []} ->
            {error, undefined};
        _ ->
            {error, db_error}
    end.
