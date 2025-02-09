%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2025 7:10 PM
%%%-------------------------------------------------------------------
-module(eschat_session_cleanup).
-author("ginleaf").

-include("eschat_session_h.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(eschat_session_cleanup_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #eschat_session_cleanup_state{}} | {ok, State :: #eschat_session_cleanup_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  erlang:send_after(?CACHE_TTL_MS, self(), check_table),
  {ok, #eschat_session_cleanup_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #eschat_session_cleanup_state{}) ->
  {reply, Reply :: term(), NewState :: #eschat_session_cleanup_state{}} |
  {reply, Reply :: term(), NewState :: #eschat_session_cleanup_state{}, timeout() | hibernate} |
  {noreply, NewState :: #eschat_session_cleanup_state{}} |
  {noreply, NewState :: #eschat_session_cleanup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #eschat_session_cleanup_state{}} |
  {stop, Reason :: term(), NewState :: #eschat_session_cleanup_state{}}).
handle_call(_Request, _From, State = #eschat_session_cleanup_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #eschat_session_cleanup_state{}) ->
  {noreply, NewState :: #eschat_session_cleanup_state{}} |
  {noreply, NewState :: #eschat_session_cleanup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eschat_session_cleanup_state{}}).
handle_cast(_Request, State = #eschat_session_cleanup_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #eschat_session_cleanup_state{}) ->
  {noreply, NewState :: #eschat_session_cleanup_state{}} |
  {noreply, NewState :: #eschat_session_cleanup_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eschat_session_cleanup_state{}}).
handle_info(check_table, State = #eschat_session_cleanup_state{}) ->
  check_table(),
  erlang:send_after(?CACHE_TTL_MS, self(), check_table),
  {noreply, State};
handle_info(_Info, State = #eschat_session_cleanup_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #eschat_session_cleanup_state{}) -> term()).
terminate(_Reason, _State = #eschat_session_cleanup_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #eschat_session_cleanup_state{},
    Extra :: term()) ->
  {ok, NewState :: #eschat_session_cleanup_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #eschat_session_cleanup_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_table() ->
  {error, not_implement}.
%%  CurrentTimeS = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
%%
%%        end.
