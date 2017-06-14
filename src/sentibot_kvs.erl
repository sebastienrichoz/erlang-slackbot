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

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([put/2, get/1, get/0]).

-record(state, {sentiMap, userSentiMap}).


%%====================================================================
%% API. Defines the methods available
%%====================================================================

% -spec put(User, Sentiment) -> #{User => Sentiment}.
put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

% -spec get(User) -> AsciiSentiment.
get(Key) ->
  gen_server:call(?MODULE, {get, Key}).

% -spec get() -> [{User, Sentiment}].
get() ->
  gen_server:call(?MODULE, {get}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Mandatory callback functions
%%====================================================================
init([]) ->
  %% Set the map of sentiments, and the map of user sentiments.
  % TODO sentimap
  SentiMap = #{happy => ":)", sad => ":("},
  UserSentiMap = #{}, % alice => happy, ...
  io:fwrite("init sentibot_ksv.~n", []),
  {ok, #state{sentiMap = SentiMap, userSentiMap = UserSentiMap}}.

handle_call({get}, _From, State) ->
  Data = kvs_get(State#state.userSentiMap),
  {reply, Data, State};

handle_call({get, Key}, _From, State) ->
  Data = kvs_get(Key, State#state.userSentiMap, State#state.sentiMap),
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

% TODO Tests (remove)
% application:start(sentibot).
% sentibot_kvs:put("Alice", happy).
% sentibot_kvs:get().


%%====================================================================
%% Internal functions
%%====================================================================
kvs_get(Key, UserSentiMap, SentiMap) ->
  Value = maps:find(Key, UserSentiMap),
  case Value of
    {ok, Sentiment} -> maps:find(Sentiment, SentiMap);
    error -> error
  end.

kvs_put(Key, Value, UserSentiMap) ->
  maps:put(Key, Value, UserSentiMap).

kvs_get(UserSentiMap) ->
  maps:to_list(UserSentiMap).