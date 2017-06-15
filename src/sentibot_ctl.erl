%%%-------------------------------------------------------------------
%%% @author sebastien
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. juin 2017 14:03
%%%-------------------------------------------------------------------
-module(sentibot_ctl).
-behaviour(gen_server).

%% API
-export([start_link/0, process/2]). % API functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % Callback functions

-record(state, {botname}).

%%====================================================================
%% API. Defines the methods available
%%====================================================================
% Process messages received from any Slack channel
process(message, EventMap) ->
  gen_server:cast(?MODULE, {message, EventMap}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Mandatory callback functions
%%====================================================================
init([]) ->
  {ok, #state{botname = "sentibot:"}}.

% Handling Sync calls
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% Handling Async calls
handle_cast({message, EventMap}, State) ->
  Newname = parse(message, EventMap, State#state.botname),
  {noreply, State#state{botname = Newname}};
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
parse(message, EventMap, Name) ->
  {ok, Text} = maps:find(<<"text">>, EventMap),
  String = string:to_lower(binary:bin_to_list(Text)),
  Chunks = string:tokens(String, " "),
  case Chunks of
    ["i", "am", Feeling | _] -> add_feeling(EventMap, Feeling), Name;
    [Name, "sentiments" | _] -> get_feelings(EventMap), Name;
    [Name, "help" | _] -> send_help(EventMap, Name), Name;
    [Name, "rename", NewName] -> lists:flatten(NewName, ":");
    _Other -> Name
  end.

add_feeling(EventMap, Feeling) ->
  case sentibot_kvs:is_member(Feeling) of
    true ->
      {ok, UserIdBin} = maps:find(<<"user">>, EventMap),
      ChannelId = get_channel(EventMap),
      Username = sentibot_slack:get_username(UserIdBin),
      UserKey = lists:flatten(["<@", binary:bin_to_list(UserIdBin), "|", Username, ">"]),
      {_, Emoji} = sentibot_kvs:put(UserKey, Feeling, ChannelId),
      sentibot_slack:send(format(UserKey, Emoji), ChannelId);
    false -> ok
  end.

get_feelings(EventMap) ->
  ChannelId = get_channel(EventMap),
  Feelings = sentibot_kvs:get(ChannelId),
  case Feelings of
    empty -> Msg = "Damn ! It looks like nobody feels anything...";
    _ -> Msg = lists:flatten(format(Feelings))
  end,
  sentibot_slack:send(Msg, ChannelId).

get_channel(EventMap) ->
  {ok, ChannelBin} = maps:find(<<"channel">>, EventMap),
  binary:bin_to_list(ChannelBin).

send_help(EventMap, Name) ->
  Channel = get_channel(EventMap),
  Feelings = sentibot_kvs:get_sentiments(),
  Msg = format_help(Feelings, Name),
  sentibot_slack:send(Msg, Channel).

% Format message 'I am happy' :
% John: I am happy => @john is :simple_smile:
format(UserKey, Emoji) ->
  lists:flatten([UserKey, " is ", Emoji]).

% Format message 'botname: sentiments' :
% John: botname: sentiments => @john is happy, @zoe is tired.
format([{User, Feeling} | T]) ->
  TupleSep = " is ",
  format_r(T, TupleSep, ", ", [User, TupleSep, Feeling, "."]);
format([]) -> ["<empty>, format is: 'I am <sentiment>'"].

format_r([{User, Feeling} | T], TupleSep, ListSep, Acc) ->
  format_r(T, TupleSep, ListSep, [User, TupleSep, Feeling, ListSep | Acc]);
format_r([], _, _, Acc) -> Acc.

% Format message 'botname: help' with markdown syntax.
format_help(Feelings, BotName) ->
  Intro = lists:flatten(["*Hi !* '", BotName, "' analyses users feelings. Here are the available commands:\n\n"]),
  Help = lists:flatten(["`", BotName, " help` : display this help.\n"]),
  Feeling = lists:flatten(["`I am <S>, where <S> := ", lists:flatten(lists:join(" | ", Feelings)), "` : save your feeling.\n"]),
  All = lists:flatten(["`", BotName, " sentiments` : all feelings of this channel.\n"]),
  Rename = lists:flatten(["`", BotName, " rename newname` : Rename _", BotName, "_ into _newname:_.\n"]),
  lists:flatten([Intro, Help, Feeling, All, Rename]).
