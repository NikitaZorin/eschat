-module(eschat_user).
-author("ginleaf").

-include("eschat_user_h.hrl").

-export([get_user/1]).
-export([new_user/1]).
-export([name/0]).


get_user(Struct) ->
  {Login, NewPassword} = get_struct(Struct),
  case from_cache(Login, NewPassword) of
    {true, #user{id = Id}} ->
      {hit, Id};
    _ ->
      {_, FunR} = eschat_pool_manage:db_runner(<<"SELECT id FROM public.\"User\" WHERE login = $1 AND pass = $2">>, [Login, NewPassword]),
      case FunR of
        {ok, [{Id} | _]} ->
          UserRec = #user{id = Id, login = Login, password = NewPassword},
          eschat_cache:to_cache(UserRec, name(), Login),
          % to_cache(UserRec),
          {miss, Id};
        {ok, []} -> {error, undefined};
        _ -> {error, db_error}
      end
  end.

new_user(Struct) ->
  {Login, NewPassword} = get_struct(Struct),
  case get_user(Struct) of
    {error, undefined} ->
      eschat_pool_manage:db_runner(<<"INSERT into \"User\" (login, pass) VALUES($1, $2) Returning id">>, [Login, NewPassword]);
    {error, Type} ->
      {error, Type};
     _ -> {error, such_user_exists}
  end.

get_struct(Struct) ->
  Login = eschat_xpath:get_val(<<"login">>, Struct),
  Password = eschat_xpath:get_val(<<"password">>, Struct),
  NewPassword = hash_password(Password),
  {Login, NewPassword}.

name() -> 
  ?MODULE.

from_cache(Login, Password) ->
    % Now = ?NOW_SEC,
    case eschat_cache:from_cache(name(), Login) of
      #user{password = Password} = Rec -> 
        {true, Rec};
      _ -> {false, undefined}
    end.

hash_password(Password) ->
  {_, Salt} = application:get_env(eschat, user_salt),
  Combined = <<Salt/binary,Password/binary>>,
  Hash = crypto:hash(sha256, Combined),
  binary:encode_hex(Hash).

