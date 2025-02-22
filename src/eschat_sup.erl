-module(eschat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
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
			id => eschat_node_session_sup,
			start => {eschat_node_session_sup, start_link, []},
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
	{ok, {{one_for_one, 10, 30}, Procs}}.
