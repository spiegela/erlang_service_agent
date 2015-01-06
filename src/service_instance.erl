-module(service_instance).

-export([list/0, start/1, stop/1]).

-include("service_agent.hrl").

%% @doc List running erlang virtual machines
-spec list() -> [#instance{}].
list() -> [ plist_to_rec(P) || P <- process_attrs(process_lines()) ].

-spec start(#instance{}) -> ok.
start(I) -> _Str = os:cmd(start_cmd(I)), ok.

-spec stop(#instance{}) -> ok.
stop(#instance{bind_addr = undefined}=I) ->
  stop(I#instance{bind_addr = os:getenv("BIND_ADDR")});
stop(#instance{name = N, bind_addr = B}) ->
  Address = list_to_atom(lists:concat([N, "@", B])),
  rpc:call(Address, init, stop, []).

%%% Internal functions

%% @priv
-spec start_cmd(#instance{}) -> string().
start_cmd(#instance{name = N}=I) ->
  Bin = "/var/vcap/packages/service_agent/bin/instance_ctl start",
  lists:concat([vars(I), " ", Bin, " ", N]).

%% @priv
-spec vars(#instance{}) -> nonempty_string().
vars(#instance{bind_addr = undefined}=I) ->
  vars(I#instance{bind_addr = os:getenv("BIND_ADDR")});
vars(#instance{bind_addr= B, cookie = C, dist_min = I, dist_max = X}) ->
  lists:concat([
    "BIND_ADDR=", B,
    " COOKIE=",   atom_to_list(C),
    " DIST_MIN=", integer_to_list(I),
    " DIST_MAX=", integer_to_list(X)
  ]).

%% @priv
-spec process_lines() -> string_list().
process_lines() ->
  ProcStr = os:cmd("ps -ef | grep \"progname erl\" | grep -v grep"),
  string:tokens(ProcStr, "\n").

%% @priv
-spec process_words(nonempty_string()) -> [string_list()].
process_words(ProcLists) ->
  [ string:tokens(C, " ") || C <- string:tokens(ProcLists, "-") ].

%% @priv
-spec process_attrs(string_list()) -> [service_attr_list()].
process_attrs(ProcLines) ->
  ProcLists = [ process_words(L) || L <- ProcLines ],
  [ attr_pairs(Attr) || Attr <- ProcLists].

%% @priv
-spec attr_pairs([string_list()]) -> service_attr_list().
attr_pairs(ProcLists) ->
  [ attr_pair(A) || A <- ProcLists, apropos_attr(A) ].

%% @priv
-spec attr_pair(string_list()) -> service_attr_input().
attr_pair(["name"|Args]) ->
  {name, lists:nth(1, Args)};
attr_pair(["setcookie"|Args]) ->
  {cookie, lists:nth(1, Args)};
attr_pair(["kernel", "inet_dist_listen_min"|Args]) ->
  {dist_min, lists:nth(1, Args)};
attr_pair(["kernel", "inet_dist_listen_max"|Args]) ->
  {dist_max, lists:nth(1, Args)}.

%% @priv
-spec apropos_attr(string_list()) -> boolean().
apropos_attr([]) ->
  false;
apropos_attr([AttrName|_]) ->
  lists:member(AttrName, ["name", "setcookie", "kernel"]).

%% @priv
-spec plist_to_rec(service_attr_list()) -> #instance{}.
plist_to_rec(Plist) ->
  lists:foldl(fun add_to_record/2, #instance{}, Plist).

%% @priv
-spec add_to_record(service_attr_input(), #instance{}) -> #instance{}.
add_to_record({name, Name}, Rec) ->
  Rec#instance{name = Name};
add_to_record({cookie, Cookie}, Rec) ->
  Rec#instance{cookie = Cookie};
add_to_record({dist_min, DistMin}, Rec) ->
  Rec#instance{dist_min = DistMin};
add_to_record({dist_max, DistMax}, Rec) ->
  Rec#instance{dist_max = DistMax}.