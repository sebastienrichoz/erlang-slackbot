%%%-------------------------------------------------------------------
%%% @author sebastien
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. juin 2017 18:01
%%%-------------------------------------------------------------------
-module(sentibot_slack).
-behaviour(gen_statem).

%% API
-export([callback_mode/0]).
-export([start_link/0]).

%%====================================================================
%% API. Defines the methods available
%%====================================================================



start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Mandatory callback functions
%%====================================================================
init([]) ->
  {ok, [{token, Token}]} = file:consult(code:priv_dir/1(sentibot) ++ "/slack.config"),

  {Pid, TeamDataMap, WsUrl} = request_ws_channel(Token),
  WsPid = make_ws_connection(WsUrl),

  {ok, connecting, #state{pid=Pid, ws_pid=WsPid, teamdata=TeamDataMap,
    token=Token, message_id=0}}.

callback_mode() ->
  erlang:error(not_implemented).
