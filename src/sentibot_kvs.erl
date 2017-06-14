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
-export([start_link/0, put/2, get/1, get/0, is_member/1]).

-record(state, {emojiMap, userSentiMap}).


%%====================================================================
%% API. Defines the methods available
%%====================================================================

% -spec put(User, Sentiment) -> #{User => Sentiment}.
put(Key, Value) ->
  {state, EmojiMap, _} = gen_server:call(?MODULE, {put, Key, Value}),
  kvs_get_emoji(Value, EmojiMap).

% -spec get(User) -> AsciiSentiment.
get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

% -spec get() -> [{User, Sentiment}].
get() ->
  gen_server:call(?MODULE, {get}).

is_member(Feeling) ->
  gen_server:call(?MODULE, {member, Feeling}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Mandatory callback functions
%%====================================================================
init([]) ->
  %% Set the map of sentiments, and the map of user sentiments.
  % TODO sentiments -> sqlite db ?
  EmojiMap = #{"happy" => ":simple_smile:", "sad" => ":cry:", "tired" => ":tired_face:", "sleeping" => ":sleeping:",
    "strong" => ":muscle:"},
  UserSentiMap = #{}, % alice => happy, ...
  io:fwrite("init sentibot_kvs.~n", []),
  {ok, #state{emojiMap = EmojiMap, userSentiMap = UserSentiMap}}.

handle_call({get}, _From, State) ->
  Data = kvs_get(State#state.userSentiMap),
  {reply, Data, State};

handle_call({member, Feeling}, _From, State) ->
  Data = kvs_member(Feeling, State#state.emojiMap),
  {reply, Data, State};

handle_call({get, Key}, _From, State) ->
  Data = kvs_get(Key, State#state.userSentiMap, State#state.emojiMap),
  {reply, Data, State};

handle_call({put, Key, Value}, _From, State) ->
  NewMap = kvs_put(Key, Value, State#state.userSentiMap),
  Reply = State#state{userSentiMap = NewMap},
  {reply, Reply, Reply};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {reply, State}.

handle_info(_Info, State) ->
  {reply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
kvs_get(Key, UserSentiMap, EmojiMap) ->
  Value = maps:find(Key, UserSentiMap),
  case Value of
    {ok, Sentiment} -> kvs_get_emoji(Sentiment, EmojiMap);
    error -> error
  end.

kvs_put(Key, Value, UserSentiMap) ->
  maps:put(Key, Value, UserSentiMap).

kvs_get(UserSentiMap) ->
  maps:to_list(UserSentiMap).

kvs_get_emoji(Key, Map) ->
  maps:find(Key, Map).

kvs_member(Feeling, Map) ->
  maps:is_key(Feeling, Map).
