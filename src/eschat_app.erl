-module(eschat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
%%-export([get_envs/0]).

start(_Type, _Args) ->
	eschat_sup:start_link().

stop(_State) ->
	ok.


%% POOLBOY
%%get_envs() ->
%%	{ok, Pools} = application:get_env(eschat, pools),
%%	Pools.