%%%-------------------------------------------------------------------
%%% @author sebastien
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. juin 2017 18:01
%%%-------------------------------------------------------------------
-module(sentibot_slack).
-behaviour(gen_server).
%% API
-export([start_link/0, get_bot_token/0, get_socket/0, send/2, get_username/1]). % API functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % Callback functions

-record(state, {token, wss}).

%%====================================================================
%% API. Defines the methods available
%%====================================================================
get_bot_token() ->
  gen_server:call(?MODULE, {get_token}).

get_socket() ->
  gen_server:call(?MODULE, {get_socket}).

% return Username
get_username(UserIdBin) ->
  gen_server:call(?MODULE, {get_user, UserIdBin}).

% Messages are sent using the token stored in the gen_server state
send(Msg, Channel) ->
  gen_server:cast(?MODULE, {send_message, Msg, Channel}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Mandatory callback functions
%%====================================================================
init([]) ->
  application:ensure_all_started(slacker),
  {ok, [{token, Token}]} = file:consult(code:priv_dir(sentibot) ++ "/slack.config"),
  WSS = connect_rtm(Token),
  {ok, #state{token = Token, wss = WSS}}.

% Handling Sync calls
handle_call({get_socket}, _From, State) ->
  {reply, State#state.wss, State};
handle_call({get_token}, _From, State) ->
  {reply, State#state.token, State};
handle_call({get_user, UserIdBin}, _From, State) ->
  Username = slacker_get_user(UserIdBin, State#state.token),
  {reply, Username, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% Handling Async calls
handle_cast({send_message, Msg, Channel}, State) ->
  slacker_send(Msg, Channel, State#state.token, [{as_user, true}]),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
% Open a connection to RTM Slack API and return the websocket
connect_rtm(Token) ->
  {ok, _Status, _Headers, Body} = slacker_rtm:connect(Token),
  [_, {_URL, WSS} | _] = Body,
  binary_to_list(WSS).

slacker_send(Msg, Channel, Token, Options) ->
  slacker_chat:post_message(Token, Channel, Msg, Options).

slacker_get_user(UserIdBin, Token) ->
  {ok, _Status, _Headers, Body} = slacker_user:info(Token, UserIdBin),
  [_Response, {_, List} | _] = Body,
  Dict = dict:from_list(List),
  {ok, NameBin} = dict:find(<<"name">>, Dict),
  binary:bin_to_list(NameBin).
