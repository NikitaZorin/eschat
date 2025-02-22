-module(eschat_node_session).
-behaviour(gen_server).

%% API
-export([start_link/2, send_message/3, stop_session/1]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(UserId, WsPid) ->
    gen_server:start_link({global, {eschat_node_session, UserId}}, ?MODULE, {UserId,WsPid}, []).

init({UserId, WsPid}) ->
    {ok, #{user_id => UserId, chats => [], ws_pid => WsPid}}.

handle_call({Node, Message}, _, state) ->
    {reply, Message, state};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({new_message, Msg, MsgId}, State) ->
    WsPid = maps:get(ws_pid, State),
    WsPid ! {deliver, Msg, MsgId},
    {noreply, State};
handle_cast(ws_closed, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


stop_session(UserId) ->
    case global:whereis_name({eschat_node_session, UserId}) of
        undefined ->
            {error, user_not_found};
        Pid ->
            gen_server:cast(Pid, ws_closed),
            ok
    end. 
send_message(UserId, Msg, MsgId) ->
    case global:whereis_name({eschat_node_session, UserId}) of
        undefined ->
            {error, user_not_found};
        Pid ->
            gen_server:cast(Pid, {new_message, Msg, MsgId}),
            ok
    end.
