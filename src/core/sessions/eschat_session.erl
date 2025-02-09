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

new_sess(UserId) ->
  SessionId = uuid:to_string(uuid:uuid4()),
  Fun = fun(ConnPid) ->
    ActiveTo = create_active_to(),
    QRes = epgsql:equery(ConnPid, <<"INSERT into \"Session\" (id, user_id,active_to) VALUES($1, $2, $3)">>, [SessionId, UserId, ActiveTo]),
    lager:debug("QRES ~p", [QRes]),
    case QRes of
      {ok, _} ->
        {ok, success};
      _ -> {error, db_error}
    end
        end,

  case eschat_pool_manage:transaction(database, Fun) of
    {error, _Type} = Err  ->
      Err;
    _ ->
      io:format("SessionID ~p~n", [SessionId]),
      {ok, SessionId}
  end.

get_sess(SessId) ->
      Fun = fun(ConnPid) ->
        QRes = epgsql:equery(ConnPid, <<"SELECT user_id, active_to FROM public.\"Session\" WHERE id = $1">>, [SessId]),
        lager:debug("QRES ~p", [QRes]),
        io:format("QRES ~p", [QRes]),
        SessRec = #session{id = SessId},

        case QRes of
          {ok, _, [{UserId, ActiveTo}|_]} -> SessRec#session{userId = UserId, activeTo = ActiveTo};
          {ok, _, []} -> {error, undefined};
          _ -> {error, db_error}
        end
            end,
      case eschat_pool_manage:transaction(database, Fun) of
        {error, _Type} = Err  ->
          Err;
        Res ->
          {ok, Res}
      end.



create_active_to() ->
  {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
  NewSecs = Secs + ?CACHE_TTL,

  calendar:now_to_datetime({MegaSecs, NewSecs, MicroSecs}).
