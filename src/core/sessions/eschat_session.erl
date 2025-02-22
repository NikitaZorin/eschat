%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2025 3:19 PM
%%%-------------------------------------------------------------------
-module(eschat_session).
-author("ginleaf").

-include("eschat_session_h.hrl").

%% API
-export([new_sess/1]).
-export([get_sess/1]).
-export([name/0]).
-export([validate_session/1]).

new_sess(UserId) ->
    Sql = <<"SELECT s.id, s.user_id, s.active_to, u.login FROM public.\"Session\" s JOIN public.\"User\" u ON s.user_id = u.id WHERE user_id = $1">>,
    case get_sess(UserId, Sql) of
        {error, undefined} ->
            SessionId = uuid:to_string(uuid:uuid4()),
            ActiveTo = create_active_to(),
            to_cache(
                <<"INSERT into \"Session\" (id, user_id,active_to) VALUES($1, $2, $3) RETURNING *">>,
                [SessionId, UserId, ActiveTo],
                UserId
            );
        {error, Type} ->
            {error, Type};
        {Status, Rec} ->
            {Status, Rec}
    end.

get_sess(Id) ->
    get_sess(
        Id,
        <<"SELECT s.id, s.user_id, s.active_to, u.login FROM public.\"Session\" s JOIN public.\"User\" u ON s.user_id = u.id WHERE s.id = $1">>
    ).
get_sess(Id, Sql) ->
    Res = eschat_cache:from_cache(name(), Id),
    % make eschat_cache:nowe() and check sessionId to delete
    case Res of
        #session{id = Id} = Rec ->
            {hit, Rec};
        #session{id = _SessionId, userId = _Id} = Rec ->
            {hit, Rec};
        _ ->
            to_cache(Sql, [Id], Id)
    end.

name() -> ?MODULE.

create_active_to() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    NewSecs = Secs + ?CACHE_TTL_MS,

    calendar:now_to_datetime({MegaSecs, NewSecs, MicroSecs}).

to_cache(Sql, List, Id) ->
    {_, FunR} = eschat_pool_manage:db_runner(Sql, List),
    case FunR of
        {ok, [{SessionId, UserId, ActiveTo, Login} | _]} ->
            SessionRec = #session{
                id = SessionId, userId = UserId, activeTo = ActiveTo, login = Login
            },
            eschat_cache:to_cache(SessionRec, name(), Id),
            {miss, SessionRec};
        {ok, []} ->
            {error, undefined};
        _ ->
            {error, db_error}
    end.

validate_session(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        undefined ->
            {error, missing_session_token};
        AuthHeader ->
            Tokens = string:tokens(binary_to_list(AuthHeader), " "),
            case Tokens of
                ["Bearer", SessionToken] ->
                    case get_sess(SessionToken) of
                        {_Status, #session{userId = UserId, login = Login} = Session} ->
                            % TimeRes = is_session_valid_by_time(ActiveTo),
                            % io:format("TimeRes ~p~n", [TimeRes]),
                            % case TimeRes of
                            %     true ->
                            %         {ok, {Session#session.userId, Session#session.login}};
                            %     false ->
                                    
                            %         delete_session(SessionToken),
                            %         {error, session_expired}
                            % end;
                            {ok, {UserId, Login}};
                        {error, Type} ->
                            {error, Type}
                    end;
                _ ->
                    {error, malformed_session_header}
            end
    end.

% is_session_valid_by_time(ActiveTo) ->
%     {Date, {H, M, S}} = ActiveTo,

%     %% Перетворюємо секунди у ціле число
%     FixedActiveTo = {Date, {H, M, trunc(S)}},
%     Now = calendar:universal_time(),
%     Time1 = calendar:datetime_to_gregorian_seconds(FixedActiveTo),
%     Time2 = calendar:datetime_to_gregorian_seconds(Now),
%     % Проверяем, не выходит ли ActiveTo за предел
%     Time1 - Time2 > 30 * 60.


delete_session(SessionId) ->
    Sql = <<"DELETE FROM public.\"Session\" WHERE id = $1">>,
    case eschat_pool_manage:db_runner(Sql, [SessionId]) of
        {ok, _Result} ->
            ets:delete(name(), SessionId),
            ok;
        _ ->
            {error, db_error}
    end.
