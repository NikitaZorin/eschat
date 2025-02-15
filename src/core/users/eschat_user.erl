-module(eschat_user).
-author("ginleaf").

-include("eschat_user_h.hrl").

%% API
% -export([name/0]).
% -export([new/0]).
% -export([now/0, traverse_fun/0]).

-export([get_user/1]).
-export([new_user/1]).
-export([name/0]).


get_user(Struct) ->
  {Login, NewPassword} = get_struct(Struct),
  Test = from_cache(Login, NewPassword),
  io:format("TEST GET22 ~p~n", [Test]),
  case Test of
    {true, #user{id = Id}} ->
      {hit, Id};
    _ ->
      {_, FunR} = eschat_pool_manage:db_runner(<<"SELECT id FROM public.\"User\" WHERE login = $1 AND passwd = $2">>, [Login, NewPassword]),
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
      eschat_pool_manage:db_runner(<<"INSERT into \"User\" (login, passwd) VALUES($1, $2)">>, [Login, NewPassword]);
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
    io:format("PASSWORD ~p~n", [Password]),
    Now = ?NOW_SEC,
    case eschat_cache:from_cache(name(), Login) of
      #user{password = Password} = Rec -> 
        {true, Rec};
      _ -> {false, undefined}
    end.

% new() ->
%   Name = name(),
%   ets:new(Name, [named_table, set, public, {keypos, #user.login}]).

% to_cache(Rec) ->
%   io:format("REc TEST ~p~n", [R])
%   ets:insert(name(), Rec).

% from_cache(Login, Password) ->
%   Now = ?NOW_SEC,
%   case ets:lookup(name(), Login) of
%     [#user{password = Password, ttl = StoredNow} = Rec] when StoredNow >= Now -> {true, Rec};
%     _ -> {false, undefined}
%   end.

% now() -> ?NOW_SEC.

% traverse_fun() ->
%   fun (Key, Now) ->
%     case ets:lookup(name(), Key) of
%       [#user{ttl = StoredNow}] when StoredNow >= Now -> ok;
%       [#user{}] -> ets:delete(name(), Key), ok;
%       [] -> ok
%     end
%   end.



hash_password(Password) ->
  {_, Salt} = application:get_env(eschat, user_salt),
  Combined = <<Salt/binary,Password/binary>>,
  Hash = crypto:hash(sha256, Combined),
  binary:encode_hex(Hash).

