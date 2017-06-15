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
-export([start_link/0, put_user/3, get/2, get/1, is_member/2, get_feelings/1, put_feeling/3]).

-record(state, {defaultEmojiMap, emojiMap, userSentiMap}).


%%====================================================================
%% API. Defines the methods available
%%====================================================================

% insert or update user's sentiment for the specified channel. Return the ascii representation of feeling
put_user(Key, Value, Channel) ->
  gen_server:call(?MODULE, {put_user, Key, Value, Channel}),
  gen_server:call(?MODULE, {get_emoji, Value, Channel}).

% Insert feeling with corresponding ascii emoji for the specified channel
put_feeling(Feeling, Emoji, Channel) ->
  gen_server:call(?MODULE, {put_sentiment, Feeling, Emoji, Channel}).

% get user emoji
get(User, Channel) ->
  gen_server:call(?MODULE, {get, User, Channel}).

% get the user-sentiment list from the specified Channel
get(Channel) ->
  gen_server:call(?MODULE, {get, Channel}).

% get feelings defined in the specified channel
get_feelings(Channel) ->
  gen_server:call(?MODULE, {feelings, Channel}).

% Return true if the specified Feeling exists, false otherwise
is_member(Feeling, Channel) ->
  gen_server:call(?MODULE, {member, Feeling, Channel}).

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
  {ok, #state{defaultEmojiMap = EmojiMap, emojiMap = #{}, userSentiMap = #{}}}.

handle_call({feelings, Channel}, _From, State) ->
  Data = kvs_get_feelings(Channel, State#state.emojiMap, State#state.defaultEmojiMap),
  {reply, Data, State};

handle_call({put_sentiment, Key, Value, Channel}, _From, State) ->
  NewMap = kvs_put_sentiment(Key, Value, Channel, State#state.emojiMap),
  Reply = State#state{emojiMap = NewMap},
  {reply, Reply, State#state{emojiMap = NewMap}};

handle_call({get, Channel}, _From, State) ->
  Data = kvs_get(Channel, State#state.userSentiMap),
  {reply, Data, State};

handle_call({get_emoji, Feeling, Channel}, _From, State) ->
  Data = kvs_get_emoji(Feeling, Channel, State#state.emojiMap, State#state.defaultEmojiMap),
  {reply, Data, State};

handle_call({member, Feeling, Channel}, _From, State) ->
  Data = kvs_member(Feeling, Channel, State#state.emojiMap, State#state.defaultEmojiMap),
  {reply, Data, State};

handle_call({get, User, Channel}, _From, State) ->
  Data = kvs_get(User, Channel, State#state.userSentiMap, State#state.emojiMap, State#state.defaultEmojiMap),
  {reply, Data, State};

handle_call({put_user, Key, Value, Channel}, _From, State) ->
  NewMap = kvs_put_user(Key, Value, Channel, State#state.userSentiMap),
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
kvs_get(Key, Channel, UserSentiMap, EmojiMap, DefaultEmojiMap) ->
  case maps:find(Channel, UserSentiMap) of
    {ok, UserMap} ->
      case maps:find(Key, UserMap) of
        {ok, Feeling} -> kvs_get_emoji(Feeling, Channel, EmojiMap, DefaultEmojiMap);
        error -> error
      end;
    error -> error
  end.

kvs_put_user(Key, Value, Channel, UserSentiMap) ->
  case maps:find(Channel, UserSentiMap) of
    {ok, Map} ->
      Map2 = maps:put(Key, Value, Map),
      maps:put(Channel, Map2, UserSentiMap);
    error ->
      maps:put(Channel, #{Key => Value}, UserSentiMap)
  end.

kvs_put_sentiment(Key, Value, Channel, EmojiMap) ->
  case maps:find(Channel, EmojiMap) of
    {ok, Map} ->
      Map2 = maps:put(Key, Value, Map),
      maps:put(Channel, Map2, EmojiMap);
    error ->
      maps:put(Channel, #{Key => Value}, EmojiMap)
  end.

kvs_get(Channel, UserSentiMap) ->
  case maps:find(Channel, UserSentiMap) of
    {ok, Map} -> maps:to_list(Map);
    error -> empty
  end.

kvs_get_emoji(Key, Channel, EmojiMap, DefaultEmojiMap) ->
  case maps:find(Channel, EmojiMap) of
    {ok, Map} ->
      case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> maps:find(Key, DefaultEmojiMap)
      end;
    error ->
      maps:find(Key, DefaultEmojiMap)
  end.

kvs_member(Feeling, Channel, EmojiMap, DefaultEmojiMap) ->
  case maps:find(Channel, EmojiMap) of
    {ok, Map} ->
      case maps:is_key(Feeling, Map) of
        true -> true;
        false -> maps:is_key(Feeling, DefaultEmojiMap)
      end;
    error -> maps:is_key(Feeling, DefaultEmojiMap)
  end.

kvs_get_feelings(Channel, EmojiMap, DefaultEmojiMap) ->
  case maps:find(Channel, EmojiMap) of
    {ok, Map} -> Keys1 = maps:keys(Map);
    error -> Keys1 = []
  end,
  Keys2 = maps:keys(DefaultEmojiMap),
  NotUnique = lists:merge(lists:sort(Keys1), lists:sort(Keys2)),
  sets:to_list(sets:from_list(NotUnique)). % remove duplicate elements