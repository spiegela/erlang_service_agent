-module(service_agent).

-behaviour(gen_server).

-include("service_agent.hrl").

%% API
-export([start_link/0, create/1, delete/1, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { leader :: string() | false }).

%%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(Instance) ->
  gen_server:call(?SERVER, {create, Instance}).

delete(Instance) ->
  gen_server:call(?SERVER, {delete, Instance}).

list() ->
  gen_server:call(?SERVER, list).

%%% gen_server callbacks

init([]) ->
  Leader = os:getenv("LEADER"),
  register_agent(Leader),
  {ok, #state{agent = Leader}.

handle_call({create, Instance}, _From, State) ->
  {reply, service_instance:start(Instance), State};
handle_call({delete, Instance}, _From, State) ->
  {reply, service_instance:stop(Instance), State};
handle_call(list, _From, State) ->
  {reply, service_instance:list(), State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(register, #state{agent = Leader}=State) ->
  register_agent(Leader),
  {noreply, State};
handle_info(deregister, #state{agent = Leader}=State) ->
  deregister_agent(Leader),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Internal functions

-spec deregister_agent(string()) -> ok.
deregister_agent(LeaderIP) -> registry_action(deregister, LeaderIP).

-spec register_agent(string()) -> ok.
register_agent(LeaderIP) -> registry_action(register, LeaderIP).

-spec registry_action(atom(), string()) -> ok.
registry_action(_Action, false) ->
  ok;
registry_action(Action, LeaderIP) ->
  try
    rpc:call(broker(LeaderIP), service_agent_registry, Action, [node()], 5000)
  catch {badrpc, timeout} ->
    erlang:send_after(?REG_INTERVAL, self(), Action)
  end.

-spec broker(string()) -> atom().
broker(IP) ->
  Broker = list_to_atom(lists:concat(["service_broker@", IP])),