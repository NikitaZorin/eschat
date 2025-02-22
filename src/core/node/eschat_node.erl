-module(eschat_node).
-author("ginleaf").

% -export([get/0]).


% get() ->
% send_message(NodeInfo, Message) ->
%     RemoteNode = maps:get(node, NodeInfo),
%     Result = rpc:call(RemoteNode, chat_server, deliver_message, [Message]),
%     case Result of
%         ok ->
%             io:format("Message delivered via erpc~n"),
%             ok;
%         {error, Reason} ->
%             io:format("Error delivering message via erpc: ~p~n", [Reason]),
%             {error, Reason}
%     end.

% get(UserId, ChatId) ->
%     Query = <<"
%         SELECT n.id 
%         FROM Node n
%         JOIN ChatMember cm ON cm.chat_id = $1
%         WHERE n.last_update > NOW() - INTERVAL '5 minutes'
%         LIMIT 1
%     ">>,
%     case pgsql:equery(my_pgsql_pool, Query, [UserId]) of
%         {ok, _, [{NodeId}]} -> {ok, NodeId};
%         {ok, _, []} -> {error, not_found};
%         {error, Reason} -> {error, Reason}
%     end.