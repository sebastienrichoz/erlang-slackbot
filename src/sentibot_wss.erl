%%%-------------------------------------------------------------------
%%% @author sebastien
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. juin 2017 18:20
%%%-------------------------------------------------------------------
-module(sentibot_wss).

-behaviour(websocket_client_handler).

-export([
  start_link/0,
  init/2,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3
]).

%%====================================================================
%% API. Defines the methods available
%%====================================================================

start_link() ->
  Socket = sentibot_slack:get_socket(),
  websocket_client:start_link(Socket, ?MODULE, []).

%%====================================================================
%% Mandatory callback functions
%%====================================================================
init([], _ConnState) ->
  io:format("init sentibot_wss~n", []),
  {ok, 0}.

% Extract message and process it
websocket_handle({text, Msg}, _ConnState, State) ->
  EventMap = jsone:decode(Msg),
  MessageType = maps:find(<<"type">>, EventMap),
  io:format("WSS:  ~p~n", [EventMap]),
  case MessageType of
    {ok, <<"message">>} -> sentibot_slack:process(message, EventMap);
    _ -> ok
  end,
  {ok, State+1};
websocket_handle(_Msg, _ConnState, State) ->
  {ok, State+1}.

websocket_info(start, _ConnState, State) ->
  {ok, State}.

websocket_terminate(Reason, _ConnState, State) ->
  io:format("Websocket closed in state ~p wih reason ~p~n",
    [State, Reason]),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================