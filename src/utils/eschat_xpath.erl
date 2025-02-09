-module(eschat_xpath).
-author("ginleaf").

-export([get_val/2, get_val/3]).

get_val(Key, Struct) ->
  get_val(Key, Struct, undefined).

get_val(Key, Struct, Default) when is_list(Struct) ->
  case lists:keyfind(Key, 1, Struct) of
    {_, Value} -> Value;
    false -> Default
  end;
get_val(Key, Struct, Default) when is_map(Struct) ->
  Map = maps:merge(#{Key => Default}, Struct),
  maps:get(Key, Map).

