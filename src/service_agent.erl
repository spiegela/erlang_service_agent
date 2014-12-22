-module(service_agent).

-behaviour(gen_server).

%% API
-export([start_link/0, create/1, delete/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(Id) ->
  gen_server:call(?SERVER, {create, Input}).

delete(Id) ->
  gen_server:call(?SERVER, {delete, Input}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({create, Input}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({delete, Input}, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_erlang_vm() ->