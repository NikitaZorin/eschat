-module(eschat_node_session_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_user_session/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {eschat_node_session,
         {eschat_node_session, start_link, []},
         transient,
         5000,
         worker,
         [eschat_node_session]}
    ],
    {ok, {{simple_one_for_one, 5, 10}, Children}}.

start_user_session(UserId, WsPid) ->
    supervisor:start_child(?MODULE, [UserId, WsPid]).
