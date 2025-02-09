-module(eschat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%%	Pools = eschat_app:get_envs(),
%%	PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
%%		PoolArgs = [{name, {local, Name}},
%%			{worker_module, eschat_notfound_h}] ++ SizeArgs,
%%		poolboy:child_spec(Name, PoolArgs, WorkerArgs)
%%												end, Pools),

	Procs = [
		#{
			id => eschat_db_sup,
			start => {eschat_db_sup, start_link, []},
			restart => permanent,
			shutdown => 5000,
			type => supervisor,
			modules => dynamic
		},
		#{
			id => eschat_web_srv,
			start => {eschat_web_srv, start_link, []},
			restart => permanent,
			shutdown => 5000,
			type => worker,
			modules => dynamic
		}
	],
%%	Procs = StaticProcs ++ PoolSpecs,
	{ok, {{one_for_one, 10, 30}, Procs}}.
