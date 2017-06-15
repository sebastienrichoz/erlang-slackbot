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
-export([start_link/0, get_socket/0, process/2]). % API functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % Callback functions

-record(state, {token, wss}).

%%====================================================================
%% API. Defines the methods available
%%====================================================================
get_socket() -> % TODO Remove ?
  gen_server:call(?MODULE, {get_socket}).

process(message, EventMap) -> % TODO remove ? and call directly the module
  gen_server:cast(?MODULE, {message, EventMap}).

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
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% Handling Async calls
handle_cast({message, EventMap}, State) ->
  parse(message, EventMap, State#state.token),
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
  % TODO check status -> error
  [_, {_URL, WSS} | _] = Body,
  binary_to_list(WSS).

parse(message, EventMap, Token) ->
  {ok, Text} = maps:find(<<"text">>, EventMap),
  String = string:to_lower(binary:bin_to_list(Text)),
  Chunks = string:tokens(String, " "),
  case Chunks of
    ["i", "am", Feeling | _] -> addFeeling(EventMap, Feeling, Token);
    ["sentibot:", "sentiments" | _] -> getFeelings(EventMap, Token);
    _Other -> ok
  end.

addFeeling(EventMap, Feeling, Token) ->
  case sentibot_kvs:is_member(Feeling) of
    true ->
      {ok, UserIdBin} = maps:find(<<"user">>, EventMap),
      {ok, ChannelIdBin} = maps:find(<<"channel">>, EventMap),
      User = get_user(UserIdBin, Token),
      {_, Emoji} = sentibot_kvs:put(User, Feeling),
      ChannelId = binary:bin_to_list(ChannelIdBin),
      slacker_chat:post_message(Token, ChannelId, format(User, Emoji), []);
    false -> ok
  end.

get_user(UserIdBin, Token) ->
  {ok, _Status, _Headers, Body} = slacker_user:info(Token, UserIdBin),
  [_Response, {_, List} | _] = Body,
  Dict = dict:from_list(List),
  {ok, NameBin} = dict:find(<<"name">>, Dict),
  binary:bin_to_list(NameBin).

getFeelings(EventMap, Token) ->
  {ok, ChannelIdBin} = maps:find(<<"channel">>, EventMap),
  Feelings = sentibot_kvs:get(),
  ChannelId = binary:bin_to_list(ChannelIdBin),
  Msg = format(Feelings),
  io:fwrite("Feelings: ~p~n", [lists:flatten(Msg)]),
  slacker_chat:post_message(Token, ChannelId, lists:flatten(Msg), []).

format(User, Emoji) ->
  string:concat(string:concat(User, " is "), Emoji).

format([{User, Feeling} | T]) ->
  TupleSep = " is ",
  format_r(T, TupleSep, ", ", [User, TupleSep, Feeling, "."]);
format([]) -> ["<empty>, format is: 'I am <sentiment>'"].

format_r([{User, Feeling} | T], TupleSep, ListSep, Acc) ->
  format_r(T, TupleSep, ListSep, [User, TupleSep, Feeling, ListSep | Acc]);
format_r([], _, _, Acc) -> Acc.
