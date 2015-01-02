-module(service_instance).

-export([list/0, start/1, stop/1]).

-include("service_agent.hrl").

%% @doc List running erlang virtual machines
-spec list() -> [#instance{}].
list() -> [ plist_to_rec(P) || P <- process_attrs(process_lines()) ].

-spec start(#instance{}) -> ok.
start(I) ->
  _Str = os:cmd(start_cmd(I)),
  ok.

-spec stop(#instance{}) -> ok.
stop(#instance{name = N}) ->
  B = os:getenv("BIND_ADDR"),
  A = list_to_atom(lists:concat([N, "@", B])),
  rpc:call(A, init, stop, []).

%%% Internal functions

%% @priv
-spec start_cmd(#instance{}) -> string().
start_cmd(#instance{name = N}=I) ->
  Bin = "/var/vcap/packages/service_agent/bin/instance_ctl start",
  lists:flatten([vars(I), " ", Bin, " ", N]).

%% @priv
-spec vars(#instance{}) -> [string()].
vars(#instance{cookie = C, dist_min = I, dist_max = X}) ->
  B = os:getenv("BIND_ADDR"),
  ["BIND_ADDR=", B, " COOKIE=", C, " DIST_MIN=", I, " DIST_MAX=", X].

%% @priv
process_lines() ->
  ProcStr = os:cmd("ps -ef | grep \"progname erl\" | grep -v grep"),
  string:tokens(ProcStr, "\n").	

%% @priv
process_attrs(ProcLines) ->
  Attrs = string:tokens(ProcLines, "-"),
  [ attr_pair(Attr) || Attr <- Attrs, apropos_attr(Attr)].

%% @priv
attr_pair(["name"|Args]) ->
  {name, lists:nth(1, Args)};
attr_pair(["setcookie"|Args]) ->
  {cookie, lists:nth(1, Args)};
attr_pair(["kernel", "inet_dist_listen_min"|Args]) ->
  {dist_min, lists:nth(1, Args)};
attr_pair(["kernel", "inet_dist_listen_max"|Args]) ->
  {dist_max, lists:nth(1, Args)}.

%% @priv
apropos_attr([AttrName|_]) ->
  lists:member(AttrName, ["name", "setcookie", "kernel"]).

%% @priv
plist_to_rec(Plist) ->
  lists:foldl(fun add_to_record/2, #instance{}, Plist).

%% @priv
add_to_record({name, Name}, Rec) ->
  Rec#instance{name = Name};
add_to_record({cookie, Cookie}, Rec) ->
  Rec#instance{cookie = Cookie};
add_to_record({dist_min, DistMin}, Rec) ->
  Rec#instance{dist_min = DistMin};
add_to_record({dist_max, DistMax}, Rec) ->
  Rec#instance{dist_max = DistMax}.