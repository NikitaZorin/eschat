%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2025 10:58 AM
%%%-------------------------------------------------------------------
-module(eschat_pool_manage).
-author("ginleaf").

%% API
-export([transaction/2]).
-export([db_runner/2]).


transaction(Key, Fun) ->
  sherlock:transaction(Key, Fun).

-spec(db_runner(Sql :: term(), Args :: list()) ->
  {ok, Res :: term()} | {error, Reason :: term()}).
db_runner(Sql, Arg) ->
  Fun = fun(ConnPid) ->
    QRes = epgsql:equery(ConnPid, Sql, Arg),
    case QRes of
      {ok, 1, _, Rows} -> {ok, Rows};
      {ok, _, Rows} -> {ok, Rows};
      {ok, _} ->
        {ok, success};
      _ -> {error, db_error}
    end
        end,
  case sherlock:transaction(database, Fun) of
    {error, _Type} = Err ->
      Err;
    Res ->
      {ok, Res}
  end.