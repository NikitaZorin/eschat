%%%-------------------------------------------------------------------
%%% @author ginleaf
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2024 11:15 PM
%%%-------------------------------------------------------------------
-module(eschat_json).
-author("ginleaf").

%% API
-export([encode/1]).
-export([decode/1]).

encode(JEterms) ->
  jsone:encode(JEterms, []).

decode(JBin) ->
  jsone:decode(JBin, []).
