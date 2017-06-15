%%%-------------------------------------------------------------------
%%% @author sebastien
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. juin 2017 21:33
%%%-------------------------------------------------------------------
-module(sentibot_kvs).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, put/3, get/2, get/1, is_member/1, get_sentiments/0]).

-record(state, {emojiMap, userSentiMap}).


%%====================================================================
%% API. Defines the methods available
%%====================================================================

% -spec put(User, Sentiment) -> #{User => Sentiment}.
% insert or update user's sentiment for the specified channel
put(Key, Value, Channel) ->
  {state, EmojiMap, _} = gen_server:call(?MODULE, {put, Key, Value, Channel}),
  kvs_get_emoji(Value, EmojiMap).

% -spec get(User) -> AsciiSentiment.
% get user emoji from the specified Channel
get(User, Channel) ->
  gen_server:call(?MODULE, {get, User, Channel}).

% -spec get(Channel) -> [{User, Sentiment}].
% get all user-sentiment from the specified Channel
get(Channel) ->
  gen_server:call(?MODULE, {get, Channel}).

get_sentiments() ->
  gen_server:call(?MODULE, {sentiments}).

% Return true if the spexified Feeling exists, false otherwise
is_member(Feeling) ->
  gen_server:call(?MODULE, {member, Feeling}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Mandatory callback functions
%%====================================================================
init([]) ->
  %% Set the map of sentiments, and the map of user sentiments.
  EmojiMap = #{
    "happy" => ":simple_smile:", "sad" => ":cry:", "tired" => ":tired_face:", "sleeping" => ":sleeping:",
    "strong" => ":muscle:"},
  UserSentiMap = #{}, % {channelID1 => #{"<@U024BE7LH|bob>" => {happy}, ...}, ...}
  {ok, #state{emojiMap = EmojiMap, userSentiMap = UserSentiMap}}.

handle_call({sentiments}, _From, State) ->
  Data = kvs_get_sentiments(State#state.emojiMap),
  {reply, Data, State};

handle_call({get, Channel}, _From, State) ->
  Data = kvs_get(Channel, State#state.userSentiMap),
  {reply, Data, State};

handle_call({member, Feeling}, _From, State) ->
  Data = kvs_member(Feeling, State#state.emojiMap),
  {reply, Data, State};

handle_call({get, Key, Channel}, _From, State) ->
  Data = kvs_get(Key, Channel, State#state.userSentiMap, State#state.emojiMap),
  {reply, Data, State};

handle_call({put, Key, Value, Channel}, _From, State) ->
  NewMap = kvs_put(Key, Value, Channel, State#state.userSentiMap),
  Reply = State#state{userSentiMap = NewMap},
  {reply, Reply, Reply};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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
kvs_get(Key, Channel, UserSentiMap, EmojiMap) ->
  case maps:find(Channel, UserSentiMap) of
    {ok, Map} ->
      case maps:find(Key, Map) of
        {ok, Sentiment} -> kvs_get_emoji(Sentiment, EmojiMap);
        error -> error
      end;
    error -> error
  end.

kvs_put(Key, Value, Channel, UserSentiMap) ->
  case maps:find(Channel, UserSentiMap) of
    {ok, Map} ->
      Map2 = maps:put(Key, Value, Map),
      maps:put(Channel, Map2, UserSentiMap);
    error ->
      maps:put(Channel, #{Key => Value}, UserSentiMap)
  end.

kvs_get(Channel, UserSentiMap) ->
  case maps:find(Channel, UserSentiMap) of
    {ok, Map} -> maps:to_list(Map);
    error -> empty
  end.

kvs_get_emoji(Key, Map) ->
  maps:find(Key, Map).

kvs_member(Feeling, Map) ->
  maps:is_key(Feeling, Map).

kvs_get_sentiments(Map) ->
  maps:keys(Map).