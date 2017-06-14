%%%-------------------------------------------------------------------
%% @doc sentibot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sentibot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), #{id => I, start => {I, start_link, []}, restart => permanent, shutdown => brutal_kill, type => Type}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  io:fwrite("Hello from sentibot_sup! Initiate children modules...~n", []),
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5}, % TODO check that
  ChildSpecs = [
    ?CHILD(sentibot_kvs, worker),
    ?CHILD(sentibot_slack, worker)
  ],
  {ok, {SupFlags, ChildSpecs}}.
%{ok, { {one_for_all, 0, 1}, []} }.


%%====================================================================
%% Internal functions
%%====================================================================
